local M = {}

-- Internal: build a list of { kind, text } items by running checks. Used by
-- both check() (which prints via vim.health) and tests (which inspect data).
local function gather()
  local Lock = require("core.pack.lock")
  local Install = require("core.pack.install")
  local items = {}
  local function add(kind, text) items[#items + 1] = { kind = kind, text = text } end

  -- git on PATH
  local git_ok = vim.fn.executable("git") == 1
  if git_ok then
    add("ok", "git found at " .. vim.fn.exepath("git"))
  else
    add("error", "git is not on PATH")
  end

  -- Lockfile
  local lock_path = Lock.path()
  if vim.fn.filereadable(lock_path) == 1 then
    local data = Lock.read()
    add("ok", ("lockfile readable (%s, %d plugins)"):format(lock_path, vim.tbl_count(data.plugins)))
  else
    add("warn", "lockfile missing at " .. lock_path)
  end

  -- Install root
  local install_root = Install.install_dir(""):gsub("/$", "")
  if vim.fn.isdirectory(install_root) == 1 then
    local count = #(vim.fn.readdir(install_root) or {})
    add("ok", ("install root exists (%s, %d entries)"):format(install_root, count))
  else
    add("warn", "install root does not exist: " .. install_root)
  end

  -- Spec validation warnings captured at pack.setup time
  local Pack = require("core.pack")
  local sw = Pack._warnings or {}
  if #sw > 0 then
    for _, w in ipairs(sw) do add("warn", "spec: " .. w) end
  else
    add("ok", "no spec validation warnings")
  end

  -- Specs ↔ on-disk
  local missing, no_tags = {}, {}
  for name, spec in pairs(Pack._specs or {}) do
    if not spec.dev then
      local dir = Install.install_dir(name)
      if vim.fn.isdirectory(dir) == 0 then
        missing[#missing + 1] = name
      elseif vim.fn.isdirectory(dir .. "/doc") == 1
         and vim.fn.filereadable(dir .. "/doc/tags") == 0 then
        no_tags[#no_tags + 1] = name
      end
    end
  end
  if #missing > 0 then
    table.sort(missing)
    add("warn", ("%d spec(s) not on disk: %s - run :Pack install"):format(#missing, table.concat(missing, ", ")))
  else
    add("ok", "every spec is installed on disk")
  end
  if #no_tags > 0 then
    table.sort(no_tags)
    add("warn", ("%d plugin(s) have doc/ but no doc/tags: %s - :help is incomplete"):format(#no_tags, table.concat(no_tags, ", ")))
  else
    add("ok", "every plugin with docs has doc/tags")
  end

  -- Orphan dirs (on-disk but not in spec)
  if vim.fn.isdirectory(install_root) == 1 then
    local orphans = {}
    for _, entry in ipairs(vim.fn.readdir(install_root)) do
      if not Pack._specs[entry] then orphans[#orphans + 1] = entry end
    end
    if #orphans > 0 then
      table.sort(orphans)
      add("warn", ("%d orphan dir(s): %s - run :Pack clean"):format(#orphans, table.concat(orphans, ", ")))
    else
      add("ok", "no orphan plugin directories")
    end
  end

  return items
end

function M.report() return gather() end

function M.check()
  vim.health.start("core.pack")
  for _, item in ipairs(gather()) do
    if item.kind == "ok"    then vim.health.ok(item.text)
    elseif item.kind == "warn"  then vim.health.warn(item.text)
    elseif item.kind == "error" then vim.health.error(item.text)
    else                              vim.health.info(item.text)
    end
  end
end

return M
