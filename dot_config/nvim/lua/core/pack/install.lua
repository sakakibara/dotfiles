local M = {}

local History = require("core.pack.history")
local Lock    = require("core.pack.lock")
local Git     = require("core.pack.git")
local Jobs    = require("core.pack.jobs")
local Version = require("core.pack.version")
local UI      = require("core.pack.ui")

M._install_root_override = nil  -- tests

local function install_root()
  if M._install_root_override then return M._install_root_override end
  return vim.fn.stdpath("data") .. "/site/pack/core/opt"
end

function M.install_dir(name)
  return install_root() .. "/" .. name
end

local function notify(msg, level)
  vim.notify(msg, level or vim.log.levels.INFO)
end

-- Resolve and check out the right ref for a freshly-cloned plugin.
local function pin_to_version(spec, dir)
  local refs = Git.list_remote_refs(dir)
  refs.default_branch = Git.default_branch(dir)
  local resolved = Version.resolve(spec.version, refs)
  if not resolved then
    return nil, "could not resolve version for " .. spec.name
  end
  local r = Git.checkout(dir, resolved.ref)
  if not r.ok then return nil, r.err end
  return Git.current_rev(dir), nil
end

-- Build hook: shell / Ex command / function. Direct sync, mirrors what
-- pack.lua's run_build did under the PackChanged autocmd.
function M.run_build(spec, path)
  local b = spec.build
  if not b or b == "" then return end
  notify(("core.pack: building %s..."):format(spec.name))
  local ok, err
  if type(b) == "function" then
    ok, err = pcall(b, { name = spec.name, path = path, spec = spec })
  elseif type(b) == "string" and b:sub(1, 1) == ":" then
    local prev = vim.fn.getcwd()
    ok, err = pcall(function()
      vim.cmd.lcd({ path, mods = { silent = true } })
      vim.cmd(b:sub(2))
    end)
    pcall(vim.cmd.lcd, { prev, mods = { silent = true } })
  elseif type(b) == "string" then
    local r = vim.system({ "sh", "-c", b }, { cwd = path, text = true }):wait()
    ok, err = (r.code == 0), r.stderr
  else
    notify(("core.pack: %s has unsupported build type %s"):format(spec.name, type(b)),
      vim.log.levels.WARN)
    return
  end
  if ok then
    notify(("core.pack: build ok for %s"):format(spec.name))
  else
    notify(("core.pack: build failed for %s: %s"):format(spec.name, tostring(err)),
      vim.log.levels.ERROR)
  end
end

function M.install_missing(specs, opts)
  opts = opts or {}
  local pool = Jobs.pool({ concurrency = opts.concurrency })
  local pending = {}

  for _, spec in ipairs(specs) do
    if spec.dev then
      -- nothing on disk; pack.lua handles dev plugins separately
    else
      local dir = M.install_dir(spec.name)
      if vim.fn.isdirectory(dir) == 1 and Git.is_repo(dir) then
        -- already installed; nothing to do
      else
        -- Clean up any pre-existing non-repo dir (e.g., interrupted prior clone)
        -- so `git clone` doesn't refuse a non-empty target.
        if vim.fn.isdirectory(dir) == 1 then
          vim.fn.delete(dir, "rf")
        end
        pending[#pending + 1] = { spec = spec, dir = dir }
        pool:add({
          cmd = { "git", "clone", "--filter=blob:none", spec.src, dir },
          tag = spec.name,
          on_done = function(r)
            if r.code ~= 0 then
              notify(("core.pack: clone failed for %s: %s"):format(spec.name, r.stderr),
                vim.log.levels.ERROR)
              return
            end
            local rev, err = pin_to_version(spec, dir)
            if not rev then
              vim.fn.delete(dir, "rf")
              notify(("core.pack: %s"):format(err), vim.log.levels.ERROR); return
            end
            Lock.set(spec.name, {
              src = spec.src, rev = rev,
              version = type(spec.version) == "string" and spec.version or nil,
            })
            M.run_build(spec, dir)
          end,
        })
      end
    end
  end

  if #pending == 0 then return end
  History.snapshot()

  local names = {}
  for _, p in ipairs(pending) do names[#names + 1] = p.spec.name end
  local view = opts.open_window and UI.progress(names, { open_window = true }) or nil

  pool:run({
    on_progress = function(done, total, last)
      if view and last and last.tag then
        view:set_status(last.tag, last.code == 0 and "ok" or "error",
          last.code == 0 and "" or "failed")
      end
      if opts.on_progress then opts.on_progress(done, total, last) end
    end,
  })
end

function M.update(specs, names, opts)
  opts = opts or {}
  local by_name = {}
  for _, s in ipairs(specs) do by_name[s.name] = s end
  if names == nil or #names == 0 then names = vim.tbl_keys(by_name) end

  local pending = {}
  local pool = Jobs.pool({ concurrency = opts.concurrency })
  for _, name in ipairs(names) do
    local spec = by_name[name]
    if spec and not spec.dev then
      local dir = M.install_dir(name)
      if Git.is_repo(dir) then
        pool:add({
          cmd = { "git", "-C", dir, "fetch", "--tags", "--prune", "origin" },
          tag = name,
          on_done = function(r)
            if r.code ~= 0 then return end
            local refs = Git.list_remote_refs(dir)
            refs.default_branch = Git.default_branch(dir)
            local resolved = Version.resolve(spec.version, refs)
            if not resolved then return end
            local rev_now = Git.current_rev(dir)
            local target_rev
            if opts.target == "lockfile" then
              local entry = Lock.get(name)
              if not entry then
                notify(("core.pack: %s not in lockfile — skipping"):format(name), vim.log.levels.WARN)
                return
              end
              target_rev = entry.rev
            else
              local target_ref = resolved.kind == "branch" and ("origin/" .. resolved.ref) or resolved.ref
              local rr = vim.system({ "git", "-C", dir, "rev-parse", target_ref }, { text = true }):wait()
              target_rev = rr.code == 0 and rr.stdout:gsub("%s+", "") or nil
            end
            if target_rev and target_rev ~= rev_now then
              local checkout_ref
              if opts.target == "lockfile" then
                checkout_ref = target_rev  -- direct sha checkout for rollback
              else
                checkout_ref = (resolved.kind == "branch") and ("origin/" .. resolved.ref) or resolved.ref
              end
              pending[#pending + 1] = { spec = spec, dir = dir, from = rev_now, to = target_rev,
                ref = resolved.ref, checkout_ref = checkout_ref }
            end
          end,
        })
      end
    end
  end
  pool:run({ on_progress = opts.on_progress })

  if #pending == 0 then notify("core.pack: nothing to update"); return end

  if opts.confirm ~= false then
    local lines = { ("core.pack: %d updates pending:"):format(#pending) }
    for _, p in ipairs(pending) do
      lines[#lines + 1] = ("  %s  %s..%s"):format(p.spec.name, p.from:sub(1, 7), p.to:sub(1, 7))
    end
    local choice = vim.fn.confirm(table.concat(lines, "\n"), "&Apply\n&Cancel", 1)
    if choice ~= 1 then notify("core.pack: cancelled"); return end
  end

  History.snapshot()
  for _, p in ipairs(pending) do
    local r = Git.checkout(p.dir, p.checkout_ref or p.ref)
    if r.ok then
      Lock.set(p.spec.name, {
        src = p.spec.src, rev = Git.current_rev(p.dir),
        version = type(p.spec.version) == "string" and p.spec.version or nil,
      })
      M.run_build(p.spec, p.dir)
    else
      notify(("core.pack: update failed for %s: %s"):format(p.spec.name, r.err),
        vim.log.levels.ERROR)
    end
  end
end

function M.clean(specs, _opts)
  local keep = {}
  for _, s in ipairs(specs) do keep[s.name] = true end
  local removed = {}
  local root = install_root()
  for _, name in ipairs(vim.fn.readdir(root) or {}) do
    if not keep[name] then
      vim.fn.delete(root .. "/" .. name, "rf")
      Lock.delete(name)
      removed[#removed + 1] = name
    end
  end
  if #removed > 0 then notify("core.pack: removed " .. table.concat(removed, ", ")) end
  return removed
end

return M
