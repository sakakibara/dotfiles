-- lua/core/lock.lua
-- Lockfile workflow for vim.pack. Pins each plugin to a specific git commit
-- so `nvim` on a fresh machine installs reproducibly.
--
-- File: <config>/pack-lock.json, same shape as lazy.nvim's lazy-lock.json:
--   { "plugin-name": { "branch": "main", "commit": "abcd1234..." }, ... }
--
-- Flow:
--   - core.pack.setup() calls apply() after install_all — any plugin whose
--     HEAD differs from the locked commit gets `git checkout <commit>`.
--   - :PackLock writes the current state of every installed plugin.
--   - :PackUpdate auto-writes after vim.pack.update completes.

local M = {}

local function pack_dir()
  return vim.fn.stdpath("data") .. "/site/pack/core/opt"
end

function M.path()
  return vim.fn.stdpath("config") .. "/pack-lock.json"
end

function M.load()
  local p = M.path()
  if vim.fn.filereadable(p) ~= 1 then return {} end
  local ok, text = pcall(vim.fn.readfile, p)
  if not ok then return {} end
  local dec_ok, data = pcall(vim.json.decode, table.concat(text, "\n"))
  if not dec_ok or type(data) ~= "table" then return {} end
  return data
end

-- Atomic write: tmp file + rename to avoid partial corruption on crash.
-- Format: one plugin per line, keys sorted — produces clean git diffs.
function M.write(entries)
  local p = M.path()
  local names = vim.tbl_keys(entries)
  table.sort(names)
  local lines = { "{" }
  for i, name in ipairs(names) do
    local e = entries[name]
    local obj = e.branch
      and ('{ "branch": %q, "commit": %q }'):format(e.branch, e.commit)
      or  ('{ "commit": %q }'):format(e.commit)
    local sep = i < #names and "," or ""
    lines[#lines + 1] = ('  %q: %s%s'):format(name, obj, sep)
  end
  lines[#lines + 1] = "}"
  local tmp = p .. ".tmp"
  vim.fn.writefile(lines, tmp)
  os.rename(tmp, p)
end

local function git(args, cwd)
  local r = vim.system(vim.list_extend({ "git" }, args), { cwd = cwd, text = true }):wait()
  if r.code ~= 0 then return nil, (r.stderr or ""):gsub("\n$", "") end
  return (r.stdout or ""):gsub("\n$", "")
end

-- Collect { [name] = { branch, commit } } for every installed plugin. Names
-- come from the caller (pack._specs keys) so we don't snapshot plugins that
-- aren't declared in the current config.
function M.snapshot(names)
  local out = {}
  for _, name in ipairs(names) do
    local dir = pack_dir() .. "/" .. name
    if vim.fn.isdirectory(dir) == 1 then
      local commit = git({ "rev-parse", "HEAD" }, dir)
      local branch = git({ "rev-parse", "--abbrev-ref", "HEAD" }, dir)
      if commit then
        out[name] = { branch = branch ~= "HEAD" and branch or nil, commit = commit }
      end
    end
  end
  return out
end

-- Marker records the mtime of the lockfile at the last successful apply.
-- If the current file's mtime matches, all plugins are already in sync and
-- we can skip ~92 `git rev-parse` subprocess calls on every boot.
local function marker_path()
  return vim.fn.stdpath("state") .. "/core.pack.lock-applied"
end

local function current_mtime()
  local st = vim.uv.fs_stat(M.path())
  return st and (tostring(st.mtime.sec) .. "." .. tostring(st.mtime.nsec)) or nil
end

local function read_marker()
  local f = io.open(marker_path(), "r")
  if not f then return nil end
  local v = f:read("*a"); f:close()
  return v and v:gsub("%s+$", "") or nil
end

local function write_marker(v)
  vim.fn.mkdir(vim.fn.fnamemodify(marker_path(), ":h"), "p")
  local f = io.open(marker_path(), "w")
  if f then f:write(v); f:close() end
end

function M.is_fresh()
  local mt = current_mtime()
  return mt ~= nil and mt == read_marker()
end

-- For every plugin in the lockfile: if HEAD differs from locked commit,
-- fetch + checkout. Runs quickly when SHAs already match.
function M.apply(entries)
  local applied, skipped, errored = 0, 0, {}
  for name, lock in pairs(entries or {}) do
    local dir = pack_dir() .. "/" .. name
    if vim.fn.isdirectory(dir) == 1 and type(lock) == "table" and lock.commit then
      local head = git({ "rev-parse", "HEAD" }, dir)
      if head == lock.commit then
        skipped = skipped + 1
      else
        -- Fetch may fail offline; checkout still works if the commit is local.
        git({ "fetch", "--quiet", "origin" }, dir)
        local ok, err = git({ "checkout", "--quiet", "--detach", lock.commit }, dir)
        if ok then
          applied = applied + 1
        else
          errored[#errored + 1] = ("%s: %s"):format(name, err or "checkout failed")
        end
      end
    end
  end
  -- Record successful application so subsequent boots short-circuit.
  if #errored == 0 then
    local mt = current_mtime()
    if mt then write_marker(mt) end
  end
  return { applied = applied, skipped = skipped, errored = errored }
end

return M
