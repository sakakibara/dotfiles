local M = {}

local History    = require("core.pack.history")
local Lock       = require("core.pack.lock")
local Git        = require("core.pack.git")
local Jobs       = require("core.pack.jobs")
local Version    = require("core.pack.version")
local Refs       = require("core.pack.refs")
local UI         = require("core.pack.ui")
local Log        = require("core.pack.log")
local BuildCache = require("core.pack.build_cache")
local Txn        = require("core.pack.txn")

local async, await = Jobs.async, Jobs.await

M._install_root_override = nil  -- tests

local function install_root()
  if M._install_root_override then return M._install_root_override end
  return vim.fn.stdpath("data") .. "/site/pack/core/opt"
end

function M.install_dir(name)
  return install_root() .. "/" .. name
end

-- Generate `doc/tags` for a freshly-installed/updated plugin so `:help
-- <topic>` resolves to its docs. lazy.nvim auto-handles this on every
-- install/update; without it, plugins ship with their .txt files but
-- no tags index, and :help silently misses everything outside the
-- already-loaded set. Idempotent — :helptags rewrites the file.
function M.generate_helptags(dir)
  local doc = dir .. "/doc"
  if vim.fn.isdirectory(doc) == 1 then
    pcall(vim.cmd.helptags, doc)
  end
end


-- Resolve and check out the right ref for a freshly-cloned plugin.
-- Lockfile takes precedence: if a SHA is recorded for this plugin we
-- check it out directly so cold installs are reproducible across
-- machines. Falls back to a fresh resolve when the lock entry is
-- missing or the SHA isn't in the cloned repo (force-pushed branch,
-- removed commit, etc.) — `:Pack update` is what recomputes.
local pin_to_version = async(function(spec, dir)
  local locked = Lock.get(spec.name)
  if locked and type(locked.rev) == "string"
    and locked.rev:match("^%x+$") and #locked.rev >= 7
  then
    local r = await(Git.checkout_sha, dir, locked.rev)
    if r.ok then return locked.rev end
    vim.notify(
      ("core.pack: %s: locked rev %s not in remote; resolving fresh"):format(
        spec.name, locked.rev:sub(1, 8)),
      vim.log.levels.WARN)
  end
  local resolved, err = await(Refs.resolve, spec, dir)
  if not resolved then
    return nil, ("%s: %s"):format(spec.name, err)
  end
  local r = await(Git.checkout_sha, dir, resolved.sha)
  if not r.ok then
    return nil, ("%s: checkout failed: %s"):format(spec.name, r.err)
  end
  return resolved.sha, nil, resolved
end)

-- Build hook: shell / Ex command / function. The shell branch is async
-- (vim.system without :wait()); Ex and function branches are inherently
-- main-thread (they execute Vimscript / Lua that touches editor state).
-- Skips re-running when BuildCache reports HEAD has already been built
-- with this command; pass opts.force = true to bypass the cache.
M.run_build = async(function(spec, path, opts)
  opts = opts or {}
  local b = spec.build
  if not b or b == "" then return end

  if not opts.force and await(BuildCache.is_fresh, path, b) then
    if opts.fidget then opts.fidget:set_status("core.pack", "cached " .. spec.name) end
    return
  end

  -- During the build, surface progress via the fidget summary. After
  -- run_build returns, the calling pool's on_progress overwrites the
  -- text — that's the intentional handoff.
  if opts.fidget then opts.fidget:set_status("core.pack", "building " .. spec.name) end

  local function fail(err)
    -- Build failure is reported but does NOT mark the plugin as
    -- failed-to-install. Clone succeeded; the Lua modules are present;
    -- the plugin should still load. Build artifacts (e.g. compiled
    -- treesitter parsers) can be regenerated separately by re-running
    -- the install hook. Marking the spec as failed would silently
    -- disable the plugin entirely on next startup — far more disruptive
    -- than the build failure (which the user will see and can address).
    vim.notify(("core.pack: %s: build failed: %s"):format(spec.name, tostring(err)),
      vim.log.levels.ERROR)
  end

  local ok = false
  if type(b) == "function" then
    -- Function-style builds need the plugin on rtp so `require()` can
    -- resolve the plugin's own modules (e.g. organ.nvim's
    -- `require("organ.grammar_install").install()`). Prepend transiently;
    -- restore after to avoid polluting rtp until packadd runs.
    --
    -- ALSO clear package.loaded entries for the plugin's namespace.
    -- Without this, an update that just checked out new code would have
    -- its build hook see the cached PREVIOUS version of the module
    -- (Lua require caches by module name; updating the file on disk
    -- doesn't invalidate the cache).
    local mod = spec.name:gsub("%.nvim$", "")
    for k in pairs(package.loaded) do
      if k == mod or k:sub(1, #mod + 1) == mod .. "." then
        package.loaded[k] = nil
      end
    end
    local prev_rtp = vim.o.runtimepath
    vim.opt.runtimepath:prepend(path)
    local pok, perr = pcall(b, { name = spec.name, path = path, spec = spec })
    vim.o.runtimepath = prev_rtp
    if pok then ok = true else fail(perr) end
  elseif type(b) == "string" and b:sub(1, 1) == ":" then
    local prev = vim.fn.getcwd()
    local pok, perr = pcall(function()
      vim.cmd.lcd({ path, mods = { silent = true } })
      vim.cmd(b:sub(2))
    end)
    pcall(vim.cmd.lcd, { prev, mods = { silent = true } })
    if pok then ok = true else fail(perr) end
  elseif type(b) == "string" then
    local r = await(function(cb)
      vim.system({ "sh", "-c", b }, { cwd = path, text = true }, function(rr)
        vim.schedule(function() cb(rr) end)
      end)
    end)
    if r.code == 0 then ok = true else fail(r.stderr) end
  else
    vim.notify(("core.pack: %s has unsupported build type %s"):format(spec.name, type(b)),
      vim.log.levels.WARN)
  end

  if ok then await(BuildCache.mark_built, path, b) end
end)

function M.install_missing(specs, opts)
  opts = opts or {}
  local pending = {}

  for _, spec in ipairs(specs) do
    if not spec.dev then
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

  async(function()
    local total = #pending
    local fully_done = 0
    await(function(cb)
      local function complete_one(spec, last_result)
        fully_done = fully_done + 1
        if view then
          view:set_status("core.pack", ("installing %d/%d"):format(fully_done, total))
        end
        if opts.on_progress then opts.on_progress(fully_done, total, last_result) end
        if fully_done == total then cb() end
      end

      local pool = Jobs.pool({ concurrency = opts.concurrency })
      for _, p in ipairs(pending) do
        pool:add({
          cmd = { "git", "clone", "--filter=blob:none", p.spec.src, p.dir },
          tag = p.spec.name,
          on_done = function(r)
            if r.code ~= 0 then
              vim.notify(("core.pack: %s: clone failed: %s"):format(p.spec.name, r.stderr),
                vim.log.levels.ERROR)
              if opts.on_failed then opts.on_failed(p.spec.name, "clone failed: " .. r.stderr) end
              return complete_one(p.spec, r)
            end
            async(function()
              local rev, err, resolved = await(pin_to_version, p.spec, p.dir)
              if not rev then
                vim.fn.delete(p.dir, "rf")
                vim.notify(("core.pack: %s"):format(err), vim.log.levels.ERROR)
                if opts.on_failed then
                  local clean_msg = err:gsub("^" .. vim.pesc(p.spec.name) .. ":%s*", "")
                  opts.on_failed(p.spec.name, clean_msg)
                end
                return complete_one(p.spec, r)
              end
              local existing = Lock.get(p.spec.name) or {}
              -- Preserve the human-readable version field. spec.version may
              -- be a vim.version.range (table) after normalize; in that case
              -- type(...) == "string" is false and we'd write nil, dropping
              -- the field on round-trip. Keep the previously-locked string
              -- instead so the lockfile stays stable.
              local entry = {
                src = p.spec.src,
                rev = rev,
                version = (type(p.spec.version) == "string" and p.spec.version)
                  or existing.version,
              }
              -- Record the resolved tag's name + SHA so we can detect
              -- force-tagging on later updates ("supply chain" check).
              if resolved and resolved.kind == "tag" then
                entry.tag_name = resolved.name
                entry.tag_sha  = resolved.sha
              end
              Lock.set(p.spec.name, entry)
              await(M.run_build, p.spec, p.dir, { fidget = view, on_failed = opts.on_failed })
              M.generate_helptags(p.dir)
              complete_one(p.spec, r)
            end)()
          end,
        })
      end
      pool:run({})
    end)
    if view then view:done("core.pack") end
    if opts.on_complete then opts.on_complete() end
  end)()
end

-- Compile-check the entry-point Lua files of a freshly-updated plugin.
-- Catches syntax errors / missing files in the new revision before the
-- user trips over them at next launch. loadfile only parses; nothing
-- runs, so this is side-effect-free even on already-loaded plugins.
local function smoke_check(dir, spec_name)
  local fails = {}

  local lua = dir .. "/lua"
  if vim.fn.isdirectory(lua) == 1 then
    local derived = spec_name:gsub("%.nvim$", "")
    local candidates = {
      lua .. "/" .. derived .. ".lua",
      lua .. "/" .. derived .. "/init.lua",
    }
    for _, c in ipairs(candidates) do
      if vim.fn.filereadable(c) == 1 then
        local fn, err = loadfile(c)
        if not fn then fails[#fails + 1] = err end
        break
      end
    end
  end

  -- plugin/<*.lua> files run automatically on :packadd; if any of them
  -- has a parse error, packadd would surface it on next launch.
  local plugin_dir = dir .. "/plugin"
  if vim.fn.isdirectory(plugin_dir) == 1 then
    for _, entry in ipairs(vim.fn.readdir(plugin_dir) or {}) do
      if entry:match("%.lua$") then
        local fn, err = loadfile(plugin_dir .. "/" .. entry)
        if not fn then fails[#fails + 1] = err end
      end
    end
  end

  if #fails > 0 then return false, table.concat(fails, "\n") end
  return true
end

-- Apply already-resolved pending updates: parallel checkouts, then
-- per-plugin lockfile + build + helptags + log.
local apply_pending = async(function(pending, opts)
  opts = opts or {}
  if #pending == 0 then
    if opts.on_complete then opts.on_complete() end
    return
  end
  History.snapshot()
  Txn.begin(pending)

  if opts.fidget then opts.fidget:set_status("core.pack", ("applying 0/%d"):format(#pending)) end
  local total = #pending
  local fully_done = 0

  await(function(cb)
    local function complete_one()
      fully_done = fully_done + 1
      if opts.fidget then opts.fidget:set_status("core.pack", ("applying %d/%d"):format(fully_done, total)) end
      if opts.on_progress then opts.on_progress(fully_done, total) end
      if fully_done == total then cb() end
    end

    local pool = Jobs.pool({ concurrency = opts.concurrency })
    for _, p in ipairs(pending) do
      pool:add({
        cmd = { "git", "-C", p.dir, "checkout", "--detach", "--quiet",
                p.target_rev or p.checkout_ref or p.ref },
        tag = p.spec.name,
        on_done = function(r)
          if r.code ~= 0 then
            vim.notify(("core.pack: %s: update failed: %s"):format(p.spec.name, r.stderr),
              vim.log.levels.ERROR)
            return complete_one()
          end
          -- HEAD is now p.target_rev. Skip a sync rev-parse; reuse the
          -- value we already resolved.
          local entry = {
            src = p.spec.src,
            rev = p.target_rev,
            version = type(p.spec.version) == "string" and p.spec.version or nil,
          }
          if p.resolved and p.resolved.kind == "tag" then
            entry.tag_name = p.resolved.name
            entry.tag_sha  = p.target_rev
          end
          Lock.set(p.spec.name, entry)
          M.run_build(p.spec, p.dir, { fidget = opts.fidget }, function()
            M.generate_helptags(p.dir)
            Log.append({
              ts = os.time(),
              kind = "update",
              name = p.spec.name,
              from = p.from,
              to = p.to,
              count = p.count or 0,
              subject = p.subject or "",
            })
            complete_one()
          end)
        end,
      })
    end
    pool:run({})
  end)

  -- Smoke pass: parse-check entry-point Lua files at the new rev. Any
  -- plugin that fails reverts to its previous SHA and we re-run its
  -- build hook so artifacts realign with the restored source.
  if opts.smoke ~= false then
    local reverted = {}
    for _, p in ipairs(pending) do
      local ok, err = smoke_check(p.dir, p.spec.name)
      if not ok then
        await(Git.checkout_sha, p.dir, p.from)
        Lock.set(p.spec.name, {
          src = p.spec.src,
          rev = p.from,
          version = type(p.spec.version) == "string" and p.spec.version or nil,
        })
        await(M.run_build, p.spec, p.dir, { fidget = opts.fidget })
        reverted[#reverted + 1] = { name = p.spec.name, err = err }
      end
    end
    if #reverted > 0 then
      local names = {}
      for _, r in ipairs(reverted) do names[#names + 1] = r.name end
      vim.notify(
        ("core.pack: reverted %d plugin(s) (smoke check failed): %s"):format(
          #reverted, table.concat(names, ", ")),
        vim.log.levels.WARN)
    end
  end

  Txn.clear()
  if opts.fidget then opts.fidget:done("core.pack") end
  if opts.on_complete then opts.on_complete() end
end)

M.apply_pending = apply_pending

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
  -- Mirror Git.default_branch's fallback: pick "main"/"master" from branches
  -- when symbolic-ref didn't set origin/HEAD (rare, but cheap to guard).
  if not default_branch then
    for _, candidate in ipairs({ "main", "master" }) do
      if vim.tbl_contains(branches, candidate) then default_branch = candidate; break end
    end
  end
  local head = sections[4]:match("([^\n]+)") or ""
  return {
    tags = tags,
    branches = branches,
    default_branch = default_branch,
    head_rev = head:gsub("%s+", ""),
  }
end

-- \037 is octal for 0x1F (the section delimiter byte). Octal escapes are
-- POSIX printf; \xNN is a bash extension that dash (Ubuntu /bin/sh) treats
-- literally — silently breaking the parser without an error code.
local RESOLVE_SCRIPT = [[
git -C "$1" tag --list
printf '\037'
git -C "$1" branch -r --format='%(refname:short)'
printf '\037'
git -C "$1" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null || true
printf '\037'
git -C "$1" rev-parse HEAD
]]

M.update = function(specs, names, opts)
  opts = opts or {}
  local by_name = {}
  for _, s in ipairs(specs) do by_name[s.name] = s end
  if names == nil or #names == 0 then names = vim.tbl_keys(by_name) end

  local targets = {}
  for _, name in ipairs(names) do
    local spec = by_name[name]
    if spec and not spec.dev then
      local dir = M.install_dir(name)
      if Git.is_repo(dir) then targets[#targets + 1] = { name = name, spec = spec, dir = dir } end
    end
  end

  if #targets == 0 then
    vim.notify("core.pack: nothing to update")
    vim.schedule(function() if opts.on_complete then opts.on_complete() end end)
    return
  end

  local fidget = (opts.open_window ~= false) and UI.fidget({ open_window = true }) or nil
  if fidget then fidget:set_status("core.pack", ("fetching 0/%d"):format(#targets)) end

  async(function()
    -- Phase 1: fetch in parallel. `--force` lets force-pushed tags
    -- arrive locally; the tag immutability check below uses the lockfile
    -- to flag any tag whose SHA changed since install.
    local fetch_pool = Jobs.pool({ concurrency = opts.concurrency })
    for _, t in ipairs(targets) do
      fetch_pool:add({
        cmd = { "git", "-C", t.dir, "fetch", "--tags", "--prune", "--force", "origin" },
        tag = t.name,
        on_done = function(r) t.fetch_ok = (r.code == 0) end,
      })
    end
    await(Jobs.await_pool, fetch_pool, {
      on_progress = function(done, total)
        if fidget then fidget:set_status("core.pack", ("fetching %d/%d"):format(done, total)) end
      end,
    })

    -- Phase 2: resolve refs + HEAD via bundled script.
    local resolve_pool = Jobs.pool({ concurrency = opts.concurrency })
    for _, t in ipairs(targets) do
      if t.fetch_ok then
        resolve_pool:add({
          cmd = { "sh", "-c", RESOLVE_SCRIPT, "_", t.dir },
          tag = t.name,
          on_done = function(r)
            if r.code == 0 then t.refs = parse_resolve_output(r.stdout or "") end
          end,
        })
      end
    end
    await(Jobs.await_pool, resolve_pool, {
      on_progress = function(done, total)
        if fidget then fidget:set_status("core.pack", ("resolving %d/%d"):format(done, total)) end
      end,
    })

    -- Pure-Lua version resolution per target.
    for _, t in ipairs(targets) do
      if t.refs then
        local resolved = Version.resolve(t.spec.version, t.refs)
        if resolved then
          t.resolved = resolved
          if resolved.kind == "default" then
            local db = t.refs.default_branch
            t.target_ref = db and ("origin/" .. db) or nil
          elseif resolved.kind == "branch" then
            t.target_ref = "origin/" .. resolved.name
          elseif resolved.kind == "tag" or resolved.kind == "commit" then
            t.target_ref = resolved.name
          end
        end
      end
    end

    -- Phase 3: rev-parse target_ref^{commit}.
    local rp_pool = Jobs.pool({ concurrency = opts.concurrency })
    for _, t in ipairs(targets) do
      if t.target_ref then
        if opts.target == "lockfile" then
          local entry = Lock.get(t.name)
          if not entry then
            vim.notify(("core.pack: %s not in lockfile — skipping"):format(t.name), vim.log.levels.WARN)
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
    await(Jobs.await_pool, rp_pool, {
      on_progress = function(done, total)
        if fidget then fidget:set_status("core.pack", ("checking %d/%d"):format(done, total)) end
      end,
    })

    -- Tag immutability check: if we previously recorded a tag's SHA
    -- and the same tag name now resolves to a different SHA, the
    -- upstream tag was force-pushed. Refuse to apply the update for
    -- that plugin and surface the mismatch so the user can investigate
    -- and act intentionally.
    local tag_skipped = {}
    for _, t in ipairs(targets) do
      if t.target_rev and t.resolved and t.resolved.kind == "tag" then
        local entry = Lock.get(t.name)
        if entry and entry.tag_name == t.resolved.name
           and entry.tag_sha and entry.tag_sha ~= t.target_rev then
          tag_skipped[#tag_skipped + 1] = ("%s (%s: %s -> %s)"):format(
            t.name, t.resolved.name,
            entry.tag_sha:sub(1, 8), t.target_rev:sub(1, 8))
          t.target_rev = nil
        end
      end
    end
    if #tag_skipped > 0 then
      vim.notify(
        ("core.pack: tag SHA mismatch (force-tagged?) — refusing %d update(s):\n  %s\n  Run :Pack uninstall && :Pack install <name> to accept the new SHA."):format(
          #tag_skipped, table.concat(tag_skipped, "\n  ")),
        vim.log.levels.WARN)
    end

    -- Build pending list (pure Lua).
    local pending = {}
    for _, t in ipairs(targets) do
      if t.target_rev and t.refs and t.target_rev ~= t.refs.head_rev then
        pending[#pending + 1] = {
          spec = t.spec, dir = t.dir, from = t.refs.head_rev, to = t.target_rev,
          ref = t.resolved and t.resolved.name or nil, checkout_ref = t.checkout_ref,
          target_rev = t.target_rev, resolved = t.resolved,
        }
      end
    end

    if #pending == 0 then
      vim.notify("core.pack: nothing to update")
      if fidget then fidget:close() end
      if opts.on_complete then opts.on_complete() end
      return
    end
    if opts.confirm == false then
      apply_pending(pending, { on_complete = opts.on_complete, fidget = fidget })
      return
    end

    -- Phase 4: per-plugin commit count + latest subject for the review
    -- buffer. Run in parallel; the review only opens once all logs land.
    if fidget then fidget:set_status("core.pack", ("logging 0/%d"):format(#pending)) end
    await(function(cb)
      local log_done = 0
      for _, p in ipairs(pending) do
        Git.log_between(p.dir, p.from, p.to, function(commits)
          commits = commits or {}
          p.count = #commits
          p.subject = commits[1] and commits[1].subject or ""
          p.ago = commits[1] and commits[1].ago or ""
          log_done = log_done + 1
          if fidget then fidget:set_status("core.pack", ("logging %d/%d"):format(log_done, #pending)) end
          if log_done == #pending then cb() end
        end)
      end
    end)

    local items = {}
    for _, p in ipairs(pending) do
      items[#items + 1] = {
        name = p.spec.name, from = p.from, to = p.to,
        count = p.count, subject = p.subject, ago = p.ago, dir = p.dir,
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
            Git.run({ "log", "--pretty=%H%x09%an%x09%ar%x09%s", p.from .. ".." .. p.to },
              p.dir, function(r)
                local log = {}
                if r.ok then
                  for line in (r.stdout or ""):gmatch("[^\n]+") do
                    local sha, author, ago, subj = line:match("^(%w+)\t([^\t]*)\t([^\t]*)\t(.*)$")
                    if sha then log[#log + 1] = { sha = sha, author = author, ago = ago, subject = subj } end
                  end
                end
                cb(log)
              end)
            return
          end
        end
        cb({})
      end,
    })
  end)()
end

-- Uninstall named plugins: remove their on-disk dirs and lockfile
-- entries. The spec itself stays in the dotfiles config — running
-- :Pack install (or just relaunching nvim) will reinstall. Useful as
-- a wipe-and-refresh when a plugin's state is broken or its build
-- artifacts need regenerating.
function M.uninstall(names)
  local removed = {}
  local missing = {}
  for _, name in ipairs(names) do
    local dir = M.install_dir(name)
    if vim.fn.isdirectory(dir) == 1 then
      vim.fn.delete(dir, "rf")
      Lock.delete(name)
      removed[#removed + 1] = name
    else
      missing[#missing + 1] = name
    end
  end
  if #removed > 0 then
    vim.notify("core.pack: uninstalled " .. table.concat(removed, ", "))
  end
  if #missing > 0 then
    vim.notify("core.pack: not installed: " .. table.concat(missing, ", "), vim.log.levels.WARN)
  end
  return removed
end

function M.clean(specs, opts)
  opts = opts or {}
  local keep = {}
  for _, s in ipairs(specs) do keep[s.name] = true end
  local root = install_root()
  local orphan_names = {}
  for _, name in ipairs(vim.fn.readdir(root) or {}) do
    if not keep[name] then orphan_names[#orphan_names + 1] = name end
  end

  local function do_remove(items)
    local removed = {}
    for _, item in ipairs(items) do
      vim.fn.delete(item.dir, "rf")
      Lock.delete(item.name)
      removed[#removed + 1] = item.name
    end
    if #removed > 0 then vim.notify("core.pack: removed " .. table.concat(removed, ", ")) end
    if opts.on_complete then vim.schedule(opts.on_complete) end
    return removed
  end

  if #orphan_names == 0 then
    vim.notify("core.pack: nothing to clean")
    if opts.on_complete then vim.schedule(opts.on_complete) end
    return {}
  end

  -- Size each orphan via parallel `du -sk`; the previous sync :wait()
  -- froze the editor for ~50 ms per orphan.
  async(function()
    local orphans = {}
    local pool = Jobs.pool({ concurrency = opts.concurrency })
    for _, name in ipairs(orphan_names) do
      local dir = root .. "/" .. name
      local entry = { name = name, dir = dir, size_kb = 0 }
      orphans[#orphans + 1] = entry
      pool:add({
        cmd = { "du", "-sk", dir },
        tag = name,
        on_done = function(r)
          entry.size_kb = tonumber(r.stdout and r.stdout:match("^(%d+)") or "0") or 0
        end,
      })
    end
    await(Jobs.await_pool, pool, {})

    if opts.confirm == false then return do_remove(orphans) end
    if opts.on_review then return opts.on_review(orphans, do_remove) end
    do_remove(orphans)
  end)()
end

return M
