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
function M.run_build(spec, path, opts)
  opts = opts or {}
  local b = spec.build
  if not b or b == "" then return end
  -- During the (synchronous) build, surface progress via the fidget summary.
  -- After run_build returns, the calling pool's on_progress overwrites the text
  -- back to "installing N/M" / "applying N/M" — that's the intentional handoff.
  if opts.fidget then opts.fidget:set_status("core.pack", "building " .. spec.name) end
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
  if not ok then
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
            M.run_build(spec, dir, { fidget = view })
          end,
        })
      end
    end
  end

  if #pending == 0 then
    vim.schedule(function() if opts.on_complete then opts.on_complete() end end)
    return
  end
  History.snapshot()

  local view
  if opts.open_window then
    view = UI.fidget({ open_window = true })
    view:set_status("core.pack", ("installing 0/%d"):format(#pending))
  end

  pool:run({
    on_progress = function(done, total, last)
      if view then
        view:set_status("core.pack", ("installing %d/%d"):format(done, total))
      end
      if opts.on_progress then opts.on_progress(done, total, last) end
    end,
    on_complete = function()
      if view then view:done("core.pack") end
      if opts.on_complete then opts.on_complete() end
    end,
  })
end

-- Helper: parse the bundled resolve-script stdout into a refs table.
-- Format: <tags>\x1f<branches>\x1f<default_branch>\x1f<head_rev>
local function parse_resolve_output(stdout)
  local sections = {}
  for section in (stdout .. "\x1f"):gmatch("([^\x1f]*)\x1f") do
    sections[#sections + 1] = section
  end
  if #sections < 4 then return nil end
  local tags, branches = {}, {}
  for tag in sections[1]:gmatch("[^\n]+") do tags[#tags + 1] = tag end
  for line in sections[2]:gmatch("[^\n]+") do
    local b = line:match("^origin/(.+)$")
    if b and b ~= "HEAD" and b ~= "" then branches[#branches + 1] = b end
  end
  local raw_db = sections[3]:match("([^\n]+)")
  local default_branch = raw_db and (raw_db:match("^origin/(.+)$") or raw_db) or nil
  local head = sections[4]:match("([^\n]+)") or ""
  return {
    tags = tags,
    branches = branches,
    default_branch = default_branch,
    head_rev = head:gsub("%s+", ""),
  }
end

local RESOLVE_SCRIPT = [[
git -C "$1" tag --list
printf '\x1f'
git -C "$1" branch -r --format='%(refname:short)'
printf '\x1f'
git -C "$1" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || true
printf '\x1f'
git -C "$1" rev-parse HEAD
]]

-- Apply already-resolved pending updates: parallel checkouts, then lockfile + build hook per plugin.
local function apply_pending(pending, opts)
  opts = opts or {}
  if #pending == 0 then
    vim.schedule(function() if opts.on_complete then opts.on_complete() end end)
    return
  end
  History.snapshot()

  if opts.fidget then opts.fidget:set_status("core.pack", ("applying 0/%d"):format(#pending)) end
  local pool = Jobs.pool({ concurrency = opts.concurrency })
  for _, p in ipairs(pending) do
    pool:add({
      cmd = { "git", "-C", p.dir, "checkout", "--detach", "--quiet", p.checkout_ref or p.ref },
      tag = p.spec.name,
      on_done = function(r)
        if r.code ~= 0 then
          notify(("core.pack: update failed for %s: %s"):format(p.spec.name, r.stderr),
            vim.log.levels.ERROR)
          return
        end
        Lock.set(p.spec.name, {
          src = p.spec.src, rev = Git.current_rev(p.dir),
          version = type(p.spec.version) == "string" and p.spec.version or nil,
        })
        M.run_build(p.spec, p.dir, { fidget = opts.fidget })
      end,
    })
  end
  pool:run({
    on_progress = function(done, total)
      if opts.fidget then opts.fidget:set_status("core.pack", ("applying %d/%d"):format(done, total)) end
      if opts.on_progress then opts.on_progress(done, total) end
    end,
    on_complete = function()
      if opts.fidget then opts.fidget:done("core.pack") end
      if opts.on_complete then opts.on_complete() end
    end,
  })
end

function M.update(specs, names, opts)
  opts = opts or {}
  local by_name = {}
  for _, s in ipairs(specs) do by_name[s.name] = s end
  if names == nil or #names == 0 then names = vim.tbl_keys(by_name) end

  -- Filter to plugins on disk.
  local targets = {}
  for _, name in ipairs(names) do
    local spec = by_name[name]
    if spec and not spec.dev then
      local dir = M.install_dir(name)
      if Git.is_repo(dir) then targets[#targets + 1] = { name = name, spec = spec, dir = dir } end
    end
  end

  if #targets == 0 then
    notify("core.pack: nothing to update")
    vim.schedule(function() if opts.on_complete then opts.on_complete() end end)
    return
  end

  local fidget = (opts.open_window ~= false) and UI.fidget({ open_window = true }) or nil
  if fidget then
    fidget:set_status("core.pack", ("fetching 0/%d"):format(#targets))
  end

  -- Phase 1: fetch in parallel.
  local fetch_pool = Jobs.pool({ concurrency = opts.concurrency })
  for _, t in ipairs(targets) do
    fetch_pool:add({
      cmd = { "git", "-C", t.dir, "fetch", "--tags", "--prune", "origin" },
      tag = t.name,
      on_done = function(r)
        t.fetch_ok = (r.code == 0)
      end,
    })
  end

  fetch_pool:run({
    on_progress = function(done, total)
      if fidget then fidget:set_status("core.pack", ("fetching %d/%d"):format(done, total)) end
    end,
    on_complete = function()
      -- Phase 2: resolve refs + HEAD in parallel via bundled script.
      local resolve_pool = Jobs.pool({ concurrency = opts.concurrency })
      for _, t in ipairs(targets) do
        if t.fetch_ok then
          resolve_pool:add({
            cmd = { "sh", "-c", RESOLVE_SCRIPT, "_", t.dir },
            tag = t.name,
            on_done = function(r)
              if r.code ~= 0 then return end
              t.refs = parse_resolve_output(r.stdout or "")
            end,
          })
        end
      end
      resolve_pool:run({
        on_progress = function(done, total)
          if fidget then fidget:set_status("core.pack", ("resolving %d/%d"):format(done, total)) end
        end,
        on_complete = function()
          -- Pure-Lua version resolution per target.
          for _, t in ipairs(targets) do
            if t.refs then
              local resolved = Version.resolve(t.spec.version, t.refs)
              if resolved then
                t.resolved = resolved
                t.target_ref = resolved.kind == "branch" and ("origin/" .. resolved.ref) or resolved.ref
              end
            end
          end
          -- Phase 3: rev-parse target_ref^{commit} in parallel.
          local rp_pool = Jobs.pool({ concurrency = opts.concurrency })
          for _, t in ipairs(targets) do
            if t.target_ref then
              if opts.target == "lockfile" then
                local entry = Lock.get(t.name)
                if not entry then
                  notify(("core.pack: %s not in lockfile — skipping"):format(t.name), vim.log.levels.WARN)
                else
                  t.target_rev = entry.rev
                  t.checkout_ref = entry.rev
                end
              else
                rp_pool:add({
                  cmd = { "git", "-C", t.dir, "rev-parse", t.target_ref .. "^{commit}" },
                  tag = t.name,
                  on_done = function(r)
                    if r.code == 0 then
                      t.target_rev = (r.stdout or ""):gsub("%s+", "")
                      t.checkout_ref = t.target_ref
                    end
                  end,
                })
              end
            end
          end
          rp_pool:run({
            on_progress = function(done, total)
              if fidget then fidget:set_status("core.pack", ("checking %d/%d"):format(done, total)) end
            end,
            on_complete = function()
              -- Build pending list (pure Lua).
              local pending = {}
              for _, t in ipairs(targets) do
                if t.target_rev and t.refs and t.target_rev ~= t.refs.head_rev then
                  pending[#pending + 1] = {
                    spec = t.spec, dir = t.dir, from = t.refs.head_rev, to = t.target_rev,
                    ref = t.resolved and t.resolved.ref or nil, checkout_ref = t.checkout_ref,
                  }
                end
              end

              if #pending == 0 then
                notify("core.pack: nothing to update")
                if fidget then fidget:close() end
                if opts.on_complete then opts.on_complete() end
                return
              end
              if opts.confirm == false then
                apply_pending(pending, { on_complete = opts.on_complete, fidget = fidget })
                return
              end

              -- Compute commit count + latest commit subject for review buffer.
              -- These are local-only `git log` calls; fast (~5 ms each), still sync
              -- per-plugin. Acceptable for the review-display path because it runs
              -- once after resolve, not per-plugin in a burst.
              for _, p in ipairs(pending) do
                local commits = Git.log_between(p.dir, p.from, p.to) or {}
                p.count = #commits
                p.subject = commits[1] and commits[1].subject or ""
              end

              local items = {}
              for _, p in ipairs(pending) do
                items[#items + 1] = {
                  name = p.spec.name, from = p.from, to = p.to,
                  count = p.count, subject = p.subject, dir = p.dir,
                  _orig = p,
                }
              end

              if fidget then fidget:close(); fidget = nil end

              local complete_fired = false
              local function fire_complete()
                if complete_fired then return end
                complete_fired = true
                if opts.on_complete then opts.on_complete() end
              end
              local apply_started = false
              UI.update_review(items, {
                open_window = opts.open_window ~= false,
                on_apply = function(list)
                  apply_started = true
                  local applied = {}
                  for _, item in ipairs(list) do applied[#applied + 1] = item._orig end
                  local apply_fidget = (opts.open_window ~= false) and UI.fidget({ open_window = true }) or nil
                  apply_pending(applied, { on_complete = fire_complete, fidget = apply_fidget })
                end,
                on_close = function()
                  if not apply_started then vim.schedule(fire_complete) end
                end,
                on_expand = function(name, cb)
                  for _, p in ipairs(pending) do
                    if p.spec.name == name then
                      local pool = Jobs.pool({ concurrency = 1 })
                      pool:add({
                        cmd = { "git", "-C", p.dir, "log", "--pretty=%H%x09%s", p.from .. ".." .. p.to },
                        tag = name,
                        on_done = function(r)
                          local log = {}
                          if r.code == 0 then
                            for line in (r.stdout or ""):gmatch("[^\n]+") do
                              local sha, subj = line:match("^(%w+)\t(.*)$")
                              if sha then log[#log + 1] = { sha = sha, subject = subj } end
                            end
                          end
                          cb(log)
                        end,
                      })
                      pool:run({})
                      return
                    end
                  end
                  cb({})
                end,
              })
            end,
          })
        end,
      })
    end,
  })
end

function M.clean(specs, opts)
  opts = opts or {}
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
  vim.schedule(function() if opts.on_complete then opts.on_complete() end end)
  return removed
end

return M
