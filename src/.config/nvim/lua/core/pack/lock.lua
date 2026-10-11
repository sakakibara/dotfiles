local M = {}

M._path_override = nil  -- tests

local function default_path()
  return vim.fn.stdpath("config") .. "/pack-lock.json"
end

function M.path()
  return M._path_override or default_path()
end

local function empty()
  return { version = 1, plugins = {} }
end

local last_err

function M.read()
  local p = M.path()
  local fd = io.open(p, "r")
  if not fd then last_err = nil; return empty() end
  local raw = fd:read("*a")
  fd:close()
  if raw == "" then last_err = nil; return empty() end
  local ok, data = pcall(vim.json.decode, raw)
  if not ok or type(data) ~= "table"
    or (data.plugins ~= nil and type(data.plugins) ~= "table")
  then
    local err = ("core.pack.lock: %s is unreadable (%s); not writing to it until it is fixed or removed")
      :format(p, ok and "not a lockfile object" or tostring(data))
    if err ~= last_err then
      last_err = err
      vim.notify(err, vim.log.levels.ERROR)
    end
    return nil, err
  end
  last_err = nil
  data.version = data.version or 1
  data.plugins = data.plugins or {}
  return data
end

-- Pretty-print the lockfile: top-level keys sorted, "plugins" object's
-- inner entries each on their own indented line in alphabetical order.
-- Per-plugin entries stay on a single line -- they're small and the diff
-- value is at the plugin-name layer.
-- Serialize a plugin entry as a single-line JSON object with keys in a
-- deterministic alphabetical order. vim.json.encode by itself doesn't
-- guarantee key order, so two encodes of the same table can produce
-- different bytes -- which would defeat the byte-equality skip in
-- M.write and cause spurious lockfile rewrites on every startup.
local function encode_entry(entry)
  local keys = vim.tbl_keys(entry); table.sort(keys)
  local parts = {}
  for _, k in ipairs(keys) do
    local v = entry[k]
    local enc = (type(v) == "table" and next(v) ~= nil and not vim.islist(v)) and encode_entry(v) or vim.json.encode(v)
    parts[#parts + 1] = ("%s:%s"):format(vim.json.encode(k), enc)
  end
  return "{" .. table.concat(parts, ",") .. "}"
end

local function pretty_encode(data)
  local plugins = data.plugins or {}
  local plugin_names = vim.tbl_keys(plugins); table.sort(plugin_names)
  local plugin_lines = {}
  for i, name in ipairs(plugin_names) do
    local sep = i < #plugin_names and "," or ""
    plugin_lines[#plugin_lines + 1] = ('    %s: %s%s'):format(
      vim.json.encode(name), encode_entry(plugins[name]), sep)
  end
  local plugins_block = #plugin_lines == 0 and "{}" or
    "{\n" .. table.concat(plugin_lines, "\n") .. "\n  }"
  -- Top-level: sort all keys, emit one per line.
  local top_keys = vim.tbl_keys(data); table.sort(top_keys)
  local top_lines = {}
  for i, k in ipairs(top_keys) do
    local sep = i < #top_keys and "," or ""
    local v = (k == "plugins") and plugins_block or vim.json.encode(data[k])
    top_lines[#top_lines + 1] = ("  %s: %s%s"):format(vim.json.encode(k), v, sep)
  end
  return "{\n" .. table.concat(top_lines, "\n") .. "\n}"
end

function M.write(data)
  local p = M.path()
  vim.fn.mkdir(vim.fn.fnamemodify(p, ":h"), "p")
  local payload = pretty_encode(data)
  -- Skip the write if the on-disk bytes are already what we'd write, so a
  -- no-op does not touch mtime or churn `git status`.
  local existing_fd = io.open(p, "r")
  if existing_fd then
    local existing = existing_fd:read("*a")
    existing_fd:close()
    if existing == payload then return end
  end
  -- Write to tempfile then rename so a crash mid-write can't corrupt the file.
  local tmp = p .. ".tmp"
  local fd = assert(io.open(tmp, "w"))
  fd:write(payload)
  fd:close()
  local ok, err = os.rename(tmp, p)
  if not ok then
    os.remove(tmp)
    error("core.pack.lock: rename failed: " .. tostring(err))
  end
end

-- In-memory buffer used while a batch is open. set/get/delete redirect
-- through it so callers don't need to distinguish; commit writes to
-- disk once at the end. Begin/commit are explicit (rather than wrapping
-- a function) so the buffered region can span async work.
M._batch = nil
M._batch_unreadable = false

function M.batch_begin()
  if M._batch then return end
  local data = M.read()
  M._batch = data or empty()
  M._batch_unreadable = data == nil
end

function M.batch_commit()
  if not M._batch then return end
  local data, unreadable = M._batch, M._batch_unreadable
  M._batch = nil
  M._batch_unreadable = false
  if not unreadable then M.write(data) end
end

function M.batch_abort()
  M._batch = nil
  M._batch_unreadable = false
end

function M.get(name)
  if M._batch then return M._batch.plugins[name] end
  local data = M.read()
  return data and data.plugins[name]
end

function M.set(name, entry)
  if M._batch then
    M._batch.plugins[name] = entry
    return true
  end
  local data, err = M.read()
  if not data then return false, err end
  data.plugins[name] = entry
  M.write(data)
  return true
end

function M.delete(name)
  if M._batch then
    M._batch.plugins[name] = nil
    return true
  end
  local data, err = M.read()
  if not data then return false, err end
  data.plugins[name] = nil
  M.write(data)
  return true
end

return M
