local T = require("tests.helpers")
local stubs = require("tests.pack.stubs")

local function sh(cmd) return vim.fn.system(cmd) end

local function make_remote()
  local dir = vim.fn.tempname()
  vim.fn.mkdir(dir, "p")
  sh({ "git", "-C", dir, "init", "-q", "-b", "main" })
  sh({ "git", "-C", dir, "config", "user.email", "t@t.t" })
  sh({ "git", "-C", dir, "config", "user.name",  "t"     })
  vim.fn.writefile({ "1" }, dir .. "/init.lua")
  sh({ "git", "-C", dir, "add", "." })
  sh({ "git", "-C", dir, "commit", "-q", "-m", "first" })
  return dir
end

local function fresh()
  package.loaded["core.pack.install"] = nil
  package.loaded["core.pack.lock"] = nil
  package.loaded["core.pack.git"] = nil
  package.loaded["core.pack.jobs"] = nil
  package.loaded["core.pack.version"] = nil
  package.loaded["core.pack.ui"] = nil
  local I = require("core.pack.install")
  local L = require("core.pack.lock")
  L._path_override = vim.fn.tempname() .. ".json"
  os.remove(L._path_override)
  I._install_root_override = vim.fn.tempname() .. "-pack"
  vim.fn.mkdir(I._install_root_override, "p")
  return I, L
end

-- Drive an async function (opts has on_complete) to completion.
local function async(fn)
  local done = false
  fn(function() done = true end)
  vim.wait(60000, function() return done end, 25)
  if not done then error("async: timed out after 60s") end
end

T.describe("core.pack.install", function()
  T.it("install_missing skips already-installed plugins", function()
    local I, L = fresh()
    local remote = make_remote()
    local spec = { name = "demo", src = "file://" .. remote }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev1 = L.get("demo").rev
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    T.eq(L.get("demo").rev, rev1)
  end)

  T.it("install_missing clones a missing plugin and writes lockfile", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    local entry = L.get("demo")
    T.truthy(entry and entry.rev and #entry.rev == 40)
    T.eq(entry.src, "file://" .. remote)
  end)

  T.it("install_missing emits progress events", function()
    local I, L = fresh()
    local r1 = make_remote(); local r2 = make_remote()
    local seen = {}
    async(function(cb) I.install_missing(
      { { name = "a", src = "file://" .. r1 }, { name = "b", src = "file://" .. r2 } },
      { on_progress = function(done, total, last) seen[#seen + 1] = { done, total, last } end, on_complete = cb })
    end)
    T.eq(seen[#seen][1], 2)
    T.eq(seen[#seen][2], 2)
  end)

  T.it("install_missing runs string build hook after install", function()
    local I, L = fresh()
    local remote = make_remote()
    local marker = vim.fn.tempname()
    async(function(cb) I.install_missing(
      { { name = "demo", src = "file://" .. remote, build = "touch " .. marker } }, { on_complete = cb })
    end)
    T.truthy(vim.fn.filereadable(marker) == 1)
  end)

  T.it("install_missing returns immediately and on_complete fires later", function()
    local I, L = fresh()
    local remote = make_remote()
    local fired = false
    I.install_missing({ { name = "demo", src = "file://" .. remote } }, {
      on_complete = function() fired = true end,
    })
    T.eq(fired, false)
    vim.wait(60000, function() return fired end, 25)
    T.eq(fired, true)
    T.truthy(L.get("demo"))
  end)

  T.it("update fetches new commits and applies when confirm=false", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    local rev_before = L.get("demo").rev
    async(function(cb) I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, { confirm = false, on_complete = cb }) end)
    T.truthy(L.get("demo").rev ~= rev_before)
  end)

  T.it("clean removes plugin not in spec list", function()
    local I, L = fresh()
    local r1 = make_remote(); local r2 = make_remote()
    async(function(cb) I.install_missing(
      { { name = "a", src = "file://" .. r1 }, { name = "b", src = "file://" .. r2 } }, { on_complete = cb })
    end)
    async(function(cb) I.clean({ { name = "a", src = "file://" .. r1 } }, { on_complete = cb }) end)
    T.eq(L.get("b"), nil)
    T.truthy(L.get("a"))
    T.eq(vim.fn.isdirectory(I.install_dir("b")), 0)
  end)

  T.it("install_missing snapshots before writing lockfile", function()
    package.loaded["core.pack.history"] = nil
    local I, L = fresh()
    local H = require("core.pack.history")
    H._dir_override = vim.fn.tempname() .. "-hist"
    vim.fn.mkdir(H._dir_override, "p")
    H._max_snapshots = 5
    local remote = make_remote()
    L.set("seed", { src = "x", rev = "0" })
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    T.truthy(#H.list() >= 1)
  end)

  T.it("update with target=lockfile checks out lockfile rev (rollback flow)", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    local rev_a = L.get("demo").rev
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    async(function(cb) I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, { confirm = false, on_complete = cb }) end)
    local rev_b = L.get("demo").rev
    T.truthy(rev_a ~= rev_b, "remote update should advance lockfile")
    L.set("demo", { src = "file://" .. remote, rev = rev_a })
    async(function(cb) I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, { confirm = false, target = "lockfile", on_complete = cb }) end)
    local Git = require("core.pack.git")
    T.eq(stubs.await(Git.current_rev, I.install_dir("demo")), rev_a)
  end)

  T.it("update with confirm=true fires on_complete only after lockfile advances", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    local rev_a = L.get("demo").rev
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })

    -- Stub UI.update_review to drive on_apply then on_close synchronously, the same way
    -- the <CR> keymap does. This reproduces the race that broke before the fix.
    -- fresh() already loaded core.pack.ui; grab that same table so install.lua's
    -- local UI upvalue sees the stub.
    local UI = require("core.pack.ui")
    UI.update_review = function(items, opts)
      opts.on_apply(items)  -- apply all
      opts.on_close()       -- mimic <CR> keymap's apply+close sequence
      return { close = function() end }
    end

    local complete_called_at_rev
    I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, {
      confirm = true, open_window = false,
      on_complete = function()
        complete_called_at_rev = L.get("demo").rev
      end,
    })
    vim.wait(60000, function() return complete_called_at_rev ~= nil end, 25)
    T.truthy(complete_called_at_rev, "on_complete must fire")
    T.truthy(complete_called_at_rev ~= rev_a, "on_complete must fire AFTER lockfile advances")
  end)

  T.it("update derefs annotated tags so a no-change is detected as no-change", function()
    local I, L = fresh()
    local remote = make_remote()
    -- Create an annotated tag at remote HEAD. Annotated tags are git objects
    -- (rev-parse returns the tag-object SHA, not the commit SHA), so without
    -- ^{commit} dereferencing, target_rev would always differ from current_rev
    -- and the plugin would falsely show as needing an update with 0 commits.
    sh({ "git", "-C", remote, "tag", "-a", "v1.0.0", "-m", "release 1.0" })
    local spec = { name = "demo", src = "file://" .. remote, version = "v1.0.0" }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev_initial = L.get("demo").rev
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    T.eq(L.get("demo").rev, rev_initial, "no rev change when already at the annotated tag")
  end)

  T.it("run_build skips when source SHA + build cmd haven't changed", function()
    local I, L = fresh()
    local remote = make_remote()
    local marker = vim.fn.tempname()
    local spec = { name = "demo", src = "file://" .. remote,
                   build = "echo x >> " .. marker }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    T.eq(#vim.fn.readfile(marker), 1, "build should run on first install")

    -- Second build at the same rev with the same command: cache hit.
    async(function(cb) I.run_build(spec, I.install_dir("demo"), {}, cb) end)
    T.eq(#vim.fn.readfile(marker), 1, "second build at same rev should be cached")

    -- force = true bypasses the cache.
    async(function(cb) I.run_build(spec, I.install_dir("demo"), { force = true }, cb) end)
    T.eq(#vim.fn.readfile(marker), 2, "force should bypass the cache")
  end)

  T.it("run_build re-runs after the source SHA changes (update path)", function()
    local I, L = fresh()
    local remote = make_remote()
    local marker = vim.fn.tempname()
    local spec = { name = "demo", src = "file://" .. remote,
                   build = "echo x >> " .. marker }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    T.eq(#vim.fn.readfile(marker), 1)

    -- Advance the remote and update the local plugin: build should re-run
    -- because HEAD moved past the cached ref.
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    T.eq(#vim.fn.readfile(marker), 2, "build should re-run after rev advance")

    -- Same rev again: cached.
    async(function(cb) I.run_build(spec, I.install_dir("demo"), {}, cb) end)
    T.eq(#vim.fn.readfile(marker), 2, "build at unchanged rev should hit cache")
  end)

  T.it("update auto-reverts a plugin whose new rev has a syntax error", function()
    local I, L = fresh()
    local remote = make_remote()
    -- Seed the remote with a valid lua module so smoke_check has something
    -- to compile-check at install time.
    vim.fn.mkdir(remote .. "/lua/demo", "p")
    vim.fn.writefile({ "return {}" }, remote .. "/lua/demo/init.lua")
    sh({ "git", "-C", remote, "add", "." })
    sh({ "git", "-C", remote, "commit", "-q", "-m", "valid module" })
    local spec = { name = "demo.nvim", src = "file://" .. remote }

    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev_a = L.get("demo.nvim").rev

    -- Push a commit with a syntactically broken init.lua.
    vim.fn.writefile({ "return { broken syntax" }, remote .. "/lua/demo/init.lua")
    sh({ "git", "-C", remote, "commit", "-aq", "-m", "broken" })

    local notified
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("smoke check failed") then
        notified = msg
      end
    end
    async(function(cb) I.update({ spec }, { "demo.nvim" }, { confirm = false, on_complete = cb }) end)
    vim.notify = orig

    T.eq(L.get("demo.nvim").rev, rev_a, "lockfile should be reverted to rev_a")
    T.eq(vim.fn.readfile(I.install_dir("demo.nvim") .. "/lua/demo/init.lua")[1],
      "return {}", "working tree should be restored to rev_a")
    T.truthy(notified, "expected a smoke-check notification")
    T.truthy(notified:match("demo.nvim"), "notification should name the reverted plugin")
  end)

  T.it("update writes a txn marker during apply and clears it on success", function()
    package.loaded["core.pack.txn"] = nil
    local Txn = require("core.pack.txn")
    Txn._path_override = vim.fn.tempname() .. ".txn.json"
    os.remove(Txn._path_override)

    local I, L = fresh()
    local remote = make_remote()
    local spec = { name = "demo", src = "file://" .. remote }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)

    -- Txn was begun and cleared; no marker remains.
    T.eq(vim.fn.filereadable(Txn._path_override), 0,
      "txn marker should be cleared after a clean apply_pending")
    Txn._path_override = nil
  end)

  T.it("txn preserves attempts across apply_pending writes (resumer bumps survive)", function()
    package.loaded["core.pack.txn"] = nil
    local Txn = require("core.pack.txn")
    Txn._path_override = vim.fn.tempname() .. ".txn.json"
    os.remove(Txn._path_override)

    -- Seed a txn directly (no apply_pending yet).
    Txn.begin({ { spec = { name = "a" }, dir = "/x", from = "f", to = "t",
                  target_rev = "t", ref = nil, checkout_ref = nil } })
    T.eq(Txn.bump_attempts(), 1)
    T.eq(Txn.bump_attempts(), 2)
    T.eq(Txn.read().attempts, 2)

    -- A subsequent apply_pending writes a fresh txn (different pending) but
    -- must NOT reset attempts — otherwise resume loops forever.
    Txn.begin({ { spec = { name = "b" }, dir = "/y", from = "f", to = "t",
                  target_rev = "t", ref = nil, checkout_ref = nil } })
    T.eq(Txn.read().attempts, 2)
    Txn._path_override = nil
  end)

  T.it("apply_pending can be resumed from a stored txn", function()
    package.loaded["core.pack.txn"] = nil
    local Txn = require("core.pack.txn")
    Txn._path_override = vim.fn.tempname() .. ".txn.json"
    os.remove(Txn._path_override)

    local I, L = fresh()
    local remote = make_remote()
    local spec = { name = "demo", src = "file://" .. remote }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev_a = L.get("demo").rev
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    -- Resolve the new rev by hand so we can build a synthetic pending entry.
    local rev_b_raw = vim.fn.system({ "git", "-C", remote, "rev-parse", "HEAD" })
    local rev_b = (rev_b_raw:gsub("%s+", ""))

    local pending = { {
      spec = spec, dir = I.install_dir("demo"),
      from = rev_a, to = rev_b, target_rev = rev_b,
    } }
    async(function(cb) I.apply_pending(pending, { on_complete = cb }) end)
    T.eq(L.get("demo").rev, rev_b, "apply_pending should advance the lockfile")
    T.eq(vim.fn.filereadable(Txn._path_override), 0)
    Txn._path_override = nil
  end)

  T.it("install records tag_name + tag_sha for a tag-pinned spec", function()
    local I, L = fresh()
    local remote = make_remote()
    sh({ "git", "-C", remote, "tag", "v1.0.0" })
    local spec = { name = "demo", src = "file://" .. remote, version = "v1.0.0" }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local entry = L.get("demo")
    T.eq(entry.tag_name, "v1.0.0")
    T.truthy(entry.tag_sha and #entry.tag_sha == 40)
    T.eq(entry.tag_sha, entry.rev)
  end)

  T.it("update refuses to apply a force-tagged tag and warns", function()
    local I, L = fresh()
    local remote = make_remote()
    sh({ "git", "-C", remote, "tag", "v1.0.0" })
    local spec = { name = "demo", src = "file://" .. remote, version = "v1.0.0" }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev_orig = L.get("demo").rev

    -- Move v1.0.0 to a different commit (force-tag).
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    sh({ "git", "-C", remote, "tag", "-f", "v1.0.0" })

    local notified
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("tag SHA mismatch") then
        notified = msg
      end
    end
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    vim.notify = orig

    T.truthy(notified, "expected force-tag warning")
    T.truthy(notified:match("v1%.0%.0"), "warning should name the tag")
    T.eq(L.get("demo").rev, rev_orig, "lockfile rev should not change on force-tag")
  end)

  T.it("update reverts when the new rev's require chain errors at runtime", function()
    local I, L = fresh()
    local remote = make_remote()
    -- A module that parses fine but errors when required.
    vim.fn.mkdir(remote .. "/lua/demo", "p")
    vim.fn.writefile({ "return {}" }, remote .. "/lua/demo/init.lua")
    sh({ "git", "-C", remote, "add", "." })
    sh({ "git", "-C", remote, "commit", "-q", "-m", "valid" })
    local spec = { name = "demo.nvim", src = "file://" .. remote }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev_a = L.get("demo.nvim").rev

    -- Push a runtime-error rev: loadfile would say "OK", require() raises.
    vim.fn.writefile({ 'error("boom at module load")', "return {}" },
      remote .. "/lua/demo/init.lua")
    sh({ "git", "-C", remote, "commit", "-aq", "-m", "boom" })

    local notified
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("smoke check failed") then
        notified = msg
      end
    end
    async(function(cb) I.update({ spec }, { "demo.nvim" }, { confirm = false, on_complete = cb }) end)
    vim.notify = orig

    T.eq(L.get("demo.nvim").rev, rev_a, "runtime-erroring rev should be reverted")
    T.truthy(notified, "expected smoke-failure warning for runtime error")
  end)

  T.it("update warns on branch rewrite when vim.g.core_pack_warn_branch_rewrite is set", function()
    local I, L = fresh()
    local remote = make_remote()
    local spec = { name = "demo", src = "file://" .. remote }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    -- Rewrite history on the remote: amend the original commit so the
    -- new HEAD is NOT a descendant of the old one.
    sh({ "git", "-C", remote, "commit", "--amend", "--allow-empty", "-q", "-m", "rewritten" })

    vim.g.core_pack_warn_branch_rewrite = true
    local notified
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("branch rewrite detected") then
        notified = msg
      end
    end
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    vim.notify = orig
    vim.g.core_pack_warn_branch_rewrite = nil

    T.truthy(notified, "expected branch-rewrite warning")
    T.truthy(notified:match("demo"), "warning should name the plugin")
  end)

  T.it("reconcile reports drift when HEAD diverges from the lockfile rev", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    local rev_a = L.get("demo").rev

    -- Manual checkout to a different SHA (simulating a partially-applied
    -- update or user-driven git operation).
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    sh({ "git", "-C", I.install_dir("demo"), "fetch", "--quiet", "origin" })
    local rev_b = (vim.fn.system({ "git", "-C", remote, "rev-parse", "HEAD" }):gsub("%s+", ""))
    sh({ "git", "-C", I.install_dir("demo"), "checkout", "--detach", "--quiet", rev_b })

    local result
    async(function(cb)
      I.reconcile({ { name = "demo", src = "file://" .. remote } }, function(drifts)
        result = drifts
        cb()
      end)
    end)
    T.eq(#result, 1)
    T.eq(result[1].name, "demo")
    T.eq(result[1].expected, rev_a)
    T.eq(result[1].actual, rev_b)
  end)

  T.it("reconcile returns empty when HEAD matches lockfile", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)

    local result
    async(function(cb)
      I.reconcile({ { name = "demo", src = "file://" .. remote } }, function(drifts)
        result = drifts
        cb()
      end)
    end)
    T.eq(#result, 0)
  end)

  T.it("update notifies when auto-inferred specs gain new commands", function()
    local I, L = fresh()
    local remote = make_remote()
    -- Seed the remote with one user command.
    vim.fn.mkdir(remote .. "/plugin", "p")
    vim.fn.writefile({ "command! OldOne call s:noop()" }, remote .. "/plugin/init.vim")
    sh({ "git", "-C", remote, "add", "." })
    sh({ "git", "-C", remote, "commit", "-q", "-m", "initial" })
    local spec = { name = "demo", src = "file://" .. remote, auto = true,
                   cmd = { "OldOne" } }  -- pre-inferred
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)

    -- New rev adds a second command.
    vim.fn.writefile(
      { "command! OldOne call s:noop()", "command! BrandNew call s:new()" },
      remote .. "/plugin/init.vim")
    sh({ "git", "-C", remote, "commit", "-aq", "-m", "add brand new" })

    local notified
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.INFO and msg:match("gained new triggers") then
        notified = msg
      end
    end
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    vim.notify = orig

    T.truthy(notified, "expected new-trigger notification")
    T.truthy(notified:match("BrandNew"), "notification should name the new command")
    T.eq(notified:match("OldOne"), nil, "should not list pre-existing commands")
  end)

  T.it("update with version='stable' tracks the highest semver tag", function()
    local I, L = fresh()
    local remote = make_remote()
    sh({ "git", "-C", remote, "tag", "v0.9.0" })
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "1.0" })
    sh({ "git", "-C", remote, "tag", "v1.0.0" })
    local spec = { name = "demo", src = "file://" .. remote, version = "stable" }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev_at_v1 = (vim.fn.system({ "git", "-C", remote, "rev-parse", "v1.0.0^{commit}" }):gsub("%s+", ""))
    T.eq(L.get("demo").rev, rev_at_v1, "should install at the highest semver tag")

    -- Push a new tag; update should advance to it.
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "1.5" })
    sh({ "git", "-C", remote, "tag", "v1.5.0" })
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    local rev_at_v15 = (vim.fn.system({ "git", "-C", remote, "rev-parse", "v1.5.0^{commit}" }):gsub("%s+", ""))
    T.eq(L.get("demo").rev, rev_at_v15, "stable should advance to the new highest tag")
  end)

  T.it("update with version='pinned' refuses to advance past the lockfile rev", function()
    local I, L = fresh()
    local remote = make_remote()
    local spec = { name = "demo", src = "file://" .. remote, version = "pinned" }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev_a = L.get("demo").rev

    -- Advance the remote; pinned should NOT pull it in.
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "advance" })
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    T.eq(L.get("demo").rev, rev_a, "pinned should hold the lockfile rev across updates")
  end)

  T.it("update with range version picks highest matching tag", function()
    local I, L = fresh()
    local remote = make_remote()
    sh({ "git", "-C", remote, "tag", "v1.0.0" })
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "1.5" })
    sh({ "git", "-C", remote, "tag", "v1.5.0" })
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "2.0" })
    sh({ "git", "-C", remote, "tag", "v2.0.0" })
    local spec = { name = "demo", src = "file://" .. remote, version = vim.version.range("1") }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    -- Already at v1.5.0 (highest in range "1"). No update should be applied.
    local rev_before = L.get("demo").rev
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    T.eq(L.get("demo").rev, rev_before, "no rev change when already at highest matching tag")
    -- Now publish v1.6.0; update should bump to it (not v2.0.0).
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "1.6" })
    sh({ "git", "-C", remote, "tag", "v1.6.0" })
    async(function(cb) I.update({ spec }, { "demo" }, { confirm = false, on_complete = cb }) end)
    local rev_after = L.get("demo").rev
    local rr = vim.fn.system({ "git", "-C", remote, "rev-parse", "v1.6.0^{commit}" })
    T.eq(rev_after, (rr:gsub("%s+", "")), "should advance to v1.6.0, not v2.0.0")
  end)
end)
