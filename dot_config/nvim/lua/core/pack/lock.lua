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

function M.write(data)
  local p = M.path()
  vim.fn.mkdir(vim.fn.fnamemodify(p, ":h"), "p")
  local payload = vim.json.encode(data)
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
