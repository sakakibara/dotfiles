local M = {}

M._path_override = nil           -- tests
M._vim_pack_path_override = nil  -- tests

local function default_path()
  return vim.fn.stdpath("config") .. "/pack-lock.json"
end

local function vim_pack_default_path()
  return vim.fn.stdpath("config") .. "/nvim-pack-lock.json"
end

function M.path()
  return M._path_override or default_path()
end

local function empty()
  return { version = 1, plugins = {} }
end

function M.read()
  local p = M.path()
  local fd = io.open(p, "r")
  if not fd then return empty() end
  local raw = fd:read("*a")
  fd:close()
  if raw == "" then return empty() end
  local ok, data = pcall(vim.json.decode, raw)
  if not ok or type(data) ~= "table" then return empty() end
  data.version = data.version or 1
  data.plugins = data.plugins or {}
  return data
end

-- Pretty-print the lockfile: top-level keys sorted, "plugins" object's
-- inner entries each on their own indented line in alphabetical order.
-- Per-plugin entries stay on a single line — they're small and the diff
-- value is at the plugin-name layer.
local function pretty_encode(data)
  local plugins = data.plugins or {}
  local plugin_names = vim.tbl_keys(plugins); table.sort(plugin_names)
  local plugin_lines = {}
  for i, name in ipairs(plugin_names) do
    local sep = i < #plugin_names and "," or ""
    plugin_lines[#plugin_lines + 1] = ('    %s: %s%s'):format(
      vim.json.encode(name), vim.json.encode(plugins[name]), sep)
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
  -- Skip the write if the on-disk bytes are already what we'd write.
  -- Avoids touching mtime / triggering `git status` churn on no-ops,
  -- and means a single format-conversion write happens once when the
  -- legacy single-line lockfile is first encountered (after that the
  -- file matches the pretty-printed payload and no-op skips kick in).
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

function M.get(name)
  return M.read().plugins[name]
end

function M.set(name, entry)
  local data = M.read()
  data.plugins[name] = entry
  M.write(data)
end

function M.delete(name)
  local data = M.read()
  data.plugins[name] = nil
  M.write(data)
end

function M.migrate_from_vim_pack()
  local existing = M.read()
  if next(existing.plugins) ~= nil then return false end
  local src_path = M._vim_pack_path_override or vim_pack_default_path()
  local fd = io.open(src_path, "r")
  if not fd then return false end
  local raw = fd:read("*a"); fd:close()
  local ok, src = pcall(vim.json.decode, raw)
  if not ok or type(src) ~= "table" or type(src.plugins) ~= "table" then return false end
  M.write({ version = 1, plugins = src.plugins })
  return true
end

return M
