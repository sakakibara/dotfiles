local M = {}
local Lock = require("core.pack.lock")

M._dir_override = nil      -- tests
M._max_snapshots = 20      -- override in tests

local function dir()
  return M._dir_override or (vim.fn.stdpath("config") .. "/pack-lock-history")
end

function M._dir() return dir() end

local function ensure_dir()
  vim.fn.mkdir(dir(), "p")
end

function M.snapshot()
  local lock_path = Lock.path()
  if vim.fn.filereadable(lock_path) ~= 1 then return nil end
  ensure_dir()
  local sec, usec = vim.uv.gettimeofday()
  local ts = sec * 1000 + math.floor(usec / 1000)
  local path = dir() .. "/" .. tostring(ts) .. ".json"
  -- Avoid collision when multiple snapshots happen in the same millisecond.
  if vim.fn.filereadable(path) == 1 then
    local i = 1
    while vim.fn.filereadable(dir() .. "/" .. ts .. "-" .. i .. ".json") == 1 do i = i + 1 end
    path = dir() .. "/" .. ts .. "-" .. i .. ".json"
  end
  vim.fn.writefile(vim.fn.readfile(lock_path, "b"), path, "b")
  M._rotate()
  return path
end

function M._rotate()
  local entries = M.list()
  for i = M._max_snapshots + 1, #entries do
    os.remove(entries[i].path)
  end
end

function M.list()
  if vim.fn.isdirectory(dir()) ~= 1 then return {} end
  local out = {}
  for _, name in ipairs(vim.fn.readdir(dir()) or {}) do
    local ts = name:match("^(%d+)")
    if ts and name:match("%.json$") then
      ts = tonumber(ts)
      out[#out + 1] = {
        ts   = ts,
        path = dir() .. "/" .. name,
        iso  = os.date("!%Y-%m-%dT%H:%M:%SZ", math.floor(ts / 1000)),
      }
    end
  end
  table.sort(out, function(a, b) return a.ts > b.ts end)
  return out
end

function M.restore(ts)
  for _, e in ipairs(M.list()) do
    if e.ts == ts then
      local raw = table.concat(vim.fn.readfile(e.path, "b"), "\n")
      local ok, data = pcall(vim.json.decode, raw)
      if ok and type(data) == "table" then
        Lock.write(data)
        return data
      end
    end
  end
  return nil
end

return M
