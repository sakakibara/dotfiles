local T = require("tests.helpers")

-- Capture the :Pack command registration via stubbed nvim_create_user_command,
-- so we can drive its callback and complete function directly without the
-- registered command actually running through nvim's command parser. The stub
-- is restored after each test.
local function with_stubbed_command(fn)
  local real = vim.api.nvim_create_user_command
  local registered  -- { name, callback, opts }
  vim.api.nvim_create_user_command = function(name, callback, opts)
    if name == "Pack" then registered = { name = name, callback = callback, opts = opts } end
  end
  local ok, err = pcall(fn, function() return registered end)
  vim.api.nvim_create_user_command = real
  if not ok then error(err) end
end

-- Build a Pack-shaped stub with enough surface area for the dispatcher to
-- compute completions (subs.update / subs.build / subs.load all read
-- Pack._specs and Pack._loaded). Methods are dot-style to match real Pack.
local function fake_pack()
  local p = {
    _specs  = {
      ["plugin-a"] = { name = "plugin-a", priority = 50, build = function() end },
      ["plugin-b"] = { name = "plugin-b", priority = 50 },
    },
    _loaded = { ["plugin-a"] = true },
  }
  function p.has(n)    return p._specs[n] ~= nil end
  function p.loaded(n) return p._loaded[n] == true end
  function p.load(_)   end
  return p
end

local function fresh_commands()
  package.loaded["core.pack.commands"] = nil
  return require("core.pack.commands")
end

T.describe("core.pack.commands :Pack dispatcher", function()
  T.it("registers a single :Pack command with bang and unbounded nargs", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local r = get()
      T.truthy(r, "registered :Pack")
      T.eq(r.opts.bang, true)
      T.eq(r.opts.nargs, "*")
      T.truthy(type(r.opts.complete) == "function")
    end)
  end)

  T.it("complete() returns subcommand list when on the first arg slot", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local complete = get().opts.complete
      -- Just typed "Pack " (or "Pack" with a trailing space): no subcommand yet.
      local out = complete("", "Pack ", 5)
      -- Should include all known subcommands.
      local set = {}; for _, n in ipairs(out) do set[n] = true end
      T.truthy(set.install,  "install in completions")
      T.truthy(set.update,   "update in completions")
      T.truthy(set.status,   "status in completions")
      T.truthy(set.profile,  "profile in completions")
    end)
  end)

  T.it("complete() filters subcommand list by arglead prefix", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local out = get().opts.complete("inst", "Pack inst", 9)
      T.eq(#out, 1)
      T.eq(out[1], "install")
    end)
  end)

  T.it("complete() respects the Pack! prefix when tokenizing", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      -- "Pack!" with a space then partial subcommand — the bang must not
      -- count as part of the subcommand name during tokenization.
      local out = get().opts.complete("up", "Pack! up", 8)
      T.eq(#out, 1)
      T.eq(out[1], "update")
    end)
  end)

  T.it("complete() forwards to subcommand.complete when past the subcommand slot", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local complete = get().opts.complete
      -- :Pack update <Tab> should complete plugin names from Pack._specs.
      local out = complete("", "Pack update ", 12)
      local set = {}; for _, n in ipairs(out) do set[n] = true end
      T.truthy(set["plugin-a"])
      T.truthy(set["plugin-b"])
    end)
  end)

  T.it("complete() filters build subcommand to plugins with build hooks", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local out = get().opts.complete("", "Pack build ", 11)
      -- plugin-a has build, plugin-b doesn't.
      T.eq(#out, 1)
      T.eq(out[1], "plugin-a")
    end)
  end)

  T.it("complete() filters load subcommand to unloaded plugins only", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local out = get().opts.complete("", "Pack load ", 10)
      -- plugin-a is _loaded, plugin-b isn't.
      T.eq(#out, 1)
      T.eq(out[1], "plugin-b")
    end)
  end)

  T.it("complete() returns empty list for unknown subcommand", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local out = get().opts.complete("", "Pack bogus ", 11)
      T.eq(out, {})
    end)
  end)

  T.it("dispatcher routes to subcommand handler with shifted fargs", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local seen
      -- Stub the registered :Pack command's callback by intercepting the
      -- subcommand it would dispatch to. Easier: test via the load
      -- subcommand which is observable through Pack.load.
      local pack = fake_pack()
      pack.load = function(name) seen = name end
      package.loaded["core.pack.commands"] = nil
      with_stubbed_command(function(get2)
        require("core.pack.commands").setup(pack)
        get2().callback({ fargs = { "load", "plugin-b" }, bang = false })
        T.eq(seen, "plugin-b")
      end)
      -- Outer get() reference unused in this nested case
      local _ = get
    end)
  end)

  T.it("dispatcher propagates the bang to the subcommand opts", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local seen_target
      -- Replace the update handler indirection: hook Install.update.
      local Install = require("core.pack.install")
      local real_update = Install.update
      Install.update = function(_, _, opts) seen_target = opts.target end
      get().callback({ fargs = { "update" }, bang = true })
      Install.update = real_update
      T.eq(seen_target, "lockfile", "bang routes update to lockfile target")
    end)
  end)

  T.it("dispatcher defaults to status when no subcommand is given", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local status_called = false
      -- Stub UI.status (which subs.status calls) to observe invocation.
      package.loaded["core.pack.ui"] = nil
      package.preload["core.pack.ui"] = function()
        return {
          status = function() status_called = true; return { close = function() end } end,
        }
      end
      get().callback({ fargs = {}, bang = false })
      package.preload["core.pack.ui"] = nil
      package.loaded["core.pack.ui"] = nil
      T.eq(status_called, true)
    end)
  end)

  T.it("update -y sets confirm=false (skip review)", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local seen
      local Install = require("core.pack.install")
      local real_update = Install.update
      Install.update = function(_specs, names, opts) seen = { names = names, opts = opts } end
      get().callback({ fargs = { "update", "-y" }, bang = false })
      Install.update = real_update
      T.eq(seen.opts.confirm, false, "-y should set confirm=false")
      T.eq(#seen.names, 0, "-y alone should yield empty names list")
    end)
  end)

  T.it("update --yes is an alias for -y", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local seen
      local Install = require("core.pack.install")
      local real_update = Install.update
      Install.update = function(_specs, _names, opts) seen = opts end
      get().callback({ fargs = { "update", "--yes" }, bang = false })
      Install.update = real_update
      T.eq(seen.confirm, false)
    end)
  end)

  T.it("update plug -y partitions flag tokens from plugin names", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local seen
      local Install = require("core.pack.install")
      local real_update = Install.update
      Install.update = function(_specs, names, opts) seen = { names = names, opts = opts } end
      get().callback({ fargs = { "update", "plugin-a", "-y", "plugin-b" }, bang = false })
      Install.update = real_update
      T.eq(seen.opts.confirm, false)
      T.eq(seen.names, { "plugin-a", "plugin-b" })
    end)
  end)

  T.it("update without -y keeps confirm=true (default review)", function()
    with_stubbed_command(function(get)
      fresh_commands().setup(fake_pack())
      local seen
      local Install = require("core.pack.install")
      local real_update = Install.update
      Install.update = function(_specs, names, opts) seen = { names = names, opts = opts } end
      get().callback({ fargs = { "update", "plugin-a" }, bang = false })
      Install.update = real_update
      T.eq(seen.opts.confirm, true)
      T.eq(seen.names, { "plugin-a" })
    end)
  end)
end)

-- Stub UI for the duration of `fn` with a noop-but-introspectable mock.
-- Provides every method the subcommand handlers might call so a single
-- preload covers all subcommands' rendering paths without cross-test leakage.
local function with_ui_stub(overrides, fn)
  package.loaded["core.pack.ui"] = nil
  package.preload["core.pack.ui"] = function()
    local stub = {
      status         = function() return { close = function() end } end,
      clean_review   = function() return { close = function() end } end,
      rollback_review = function() return { close = function() end } end,
      fidget         = function()
        return {
          set_status = function() end,
          done       = function() end,
          error      = function() end,
          close      = function() end,
        }
      end,
      _active_splash = nil,
    }
    for k, v in pairs(overrides or {}) do stub[k] = v end
    return stub
  end
  local ok, err = pcall(fn)
  package.preload["core.pack.ui"] = nil
  package.loaded["core.pack.ui"] = nil
  if not ok then error(err) end
end

local function capture_notifies(fn)
  local notifications = {}
  local orig = vim.notify
  vim.notify = function(msg, lvl) notifications[#notifications + 1] = { msg = tostring(msg), level = lvl } end
  local ok, err = pcall(fn, notifications)
  vim.notify = orig
  if not ok then error(err) end
  return notifications
end

T.describe("core.pack.commands :Pack clean", function()
  T.it("on_review fires UI.clean_review whose on_apply forwards to do_remove", function()
    with_stubbed_command(function(get)
      local Install = require("core.pack.install")
      local orig_clean = Install.clean
      local removed_with
      Install.clean = function(_specs, opts)
        local sample_orphans = { { name = "ghost", size_kb = 100 } }
        opts.on_review(sample_orphans, function(list) removed_with = list end)
      end
      with_ui_stub({
        clean_review = function(orphans, ropts)
          ropts.on_apply(orphans)  -- simulate user pressing apply
          return { close = function() end }
        end,
      }, function()
        fresh_commands().setup(fake_pack())
        local ok, err = pcall(get().callback, { fargs = { "clean" }, bang = false })
        T.eq(ok, true, ":Pack clean must not raise: " .. tostring(err))
      end)
      Install.clean = orig_clean
      T.truthy(removed_with, "do_remove should have been called via on_apply")
      T.eq(#removed_with, 1)
      T.eq(removed_with[1].name, "ghost")
    end)
  end)

  T.it("clean -y bypasses UI.clean_review and applies directly", function()
    with_stubbed_command(function(get)
      local Install = require("core.pack.install")
      local orig_clean = Install.clean
      local removed_with
      Install.clean = function(_specs, opts)
        local sample_orphans = { { name = "ghost", size_kb = 100 } }
        opts.on_review(sample_orphans, function(list) removed_with = list end)
      end
      local review_called = false
      with_ui_stub({
        clean_review = function(orphans, ropts)
          review_called = true
          ropts.on_apply(orphans)
          return { close = function() end }
        end,
      }, function()
        fresh_commands().setup(fake_pack())
        local ok, err = pcall(get().callback, { fargs = { "clean", "-y" }, bang = false })
        T.eq(ok, true, ":Pack clean -y must not raise: " .. tostring(err))
      end)
      Install.clean = orig_clean
      T.eq(review_called, false, "UI.clean_review must not be called with -y")
      T.truthy(removed_with, "do_remove should still receive the orphan list")
      T.eq(#removed_with, 1)
      T.eq(removed_with[1].name, "ghost")
    end)
  end)
end)

T.describe("core.pack.commands :Pack rollback", function()
  T.it("warns when there are no snapshots", function()
    with_stubbed_command(function(get)
      package.loaded["core.pack.history"] = nil
      package.preload["core.pack.history"] = function() return { list = function() return {} end } end
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        local notes = capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "rollback" }, bang = false })
          T.eq(ok, true, ":Pack rollback must not raise: " .. tostring(err))
        end)
        local found
        for _, n in ipairs(notes) do
          if n.msg:match("no snapshots") then found = n; break end
        end
        T.truthy(found, "expected 'no snapshots' notify, got: " .. vim.inspect(notes))
        T.eq(found.level, vim.log.levels.WARN)
      end)
      package.preload["core.pack.history"] = nil
      package.loaded["core.pack.history"] = nil
    end)
  end)

  T.it("numeric arg restores the snapshot at that index", function()
    with_stubbed_command(function(get)
      local restored_ts
      package.loaded["core.pack.history"] = nil
      package.preload["core.pack.history"] = function()
        return {
          list = function()
            return { { ts = 1700000000, iso = "2024-01-01", path = "/dev/null" } }
          end,
          restore = function(ts) restored_ts = ts; return { plugins = {} } end,
        }
      end
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "rollback", "1" }, bang = false })
          T.eq(ok, true, ":Pack rollback 1 must not raise: " .. tostring(err))
        end)
      end)
      package.preload["core.pack.history"] = nil
      package.loaded["core.pack.history"] = nil
      T.eq(restored_ts, 1700000000)
    end)
  end)

  T.it("warns when numeric arg is out of range", function()
    with_stubbed_command(function(get)
      package.loaded["core.pack.history"] = nil
      package.preload["core.pack.history"] = function()
        return {
          list = function()
            return { { ts = 1, iso = "x", path = "/dev/null" } }
          end,
          restore = function() return { plugins = {} } end,
        }
      end
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        local notes = capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "rollback", "99" }, bang = false })
          T.eq(ok, true)
        end)
        local found
        for _, n in ipairs(notes) do
          if n.msg:match("no snapshot at index") then found = n; break end
        end
        T.truthy(found, "expected 'no snapshot at index' notify, got: " .. vim.inspect(notes))
      end)
      package.preload["core.pack.history"] = nil
      package.loaded["core.pack.history"] = nil
    end)
  end)

  T.it("UI on_select callback restores the chosen snapshot", function()
    with_stubbed_command(function(get)
      local restored_ts
      package.loaded["core.pack.history"] = nil
      package.preload["core.pack.history"] = function()
        return {
          list = function()
            return { { ts = 42, iso = "iso", path = "/dev/null" } }
          end,
          restore = function(ts) restored_ts = ts; return { plugins = {} } end,
        }
      end
      with_ui_stub({
        rollback_review = function(entries, ropts)
          ropts.on_select(entries[1])  -- simulate user selecting first entry
          return { close = function() end }
        end,
      }, function()
        fresh_commands().setup(fake_pack())
        capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "rollback" }, bang = false })
          T.eq(ok, true, ":Pack rollback must not raise: " .. tostring(err))
        end)
      end)
      package.preload["core.pack.history"] = nil
      package.loaded["core.pack.history"] = nil
      T.eq(restored_ts, 42)
    end)
  end)
end)

T.describe("core.pack.commands :Pack build", function()
  T.it("notifies when no plugins with build hooks", function()
    with_stubbed_command(function(get)
      local p = fake_pack()
      -- Strip build hooks to make total=0 path fire.
      p._specs["plugin-a"].build = nil
      with_ui_stub({}, function()
        fresh_commands().setup(p)
        local notes = capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "build" }, bang = false })
          T.eq(ok, true, ":Pack build must not raise: " .. tostring(err))
        end)
        local found
        for _, n in ipairs(notes) do
          if n.msg:match("no plugins with build hooks") then found = n; break end
        end
        T.truthy(found, "expected 'no plugins with build hooks' notify, got: " .. vim.inspect(notes))
      end)
    end)
  end)

  T.it("invokes Install.run_build for each spec with a build hook", function()
    with_stubbed_command(function(get)
      local Install = require("core.pack.install")
      local orig_rb, orig_id = Install.run_build, Install.install_dir
      local built = {}
      Install.run_build = function(spec, _dir, _opts) built[#built + 1] = spec.name end
      Install.install_dir = function(name) return "/tmp/" .. name end
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "build" }, bang = false })
          T.eq(ok, true, ":Pack build must not raise: " .. tostring(err))
        end)
      end)
      Install.run_build, Install.install_dir = orig_rb, orig_id
      T.eq(built, { "plugin-a" })  -- only plugin-a has a build hook in fake_pack
    end)
  end)

  T.it("notifies when target name has no build hook", function()
    with_stubbed_command(function(get)
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        local notes = capture_notifies(function()
          -- plugin-b has no build hook
          local ok, err = pcall(get().callback, { fargs = { "build", "plugin-b" }, bang = false })
          T.eq(ok, true)
        end)
        local found
        for _, n in ipairs(notes) do
          if n.msg:match("no build hook for plugin%-b") then found = n; break end
        end
        T.truthy(found, "expected 'no build hook for plugin-b' notify, got: " .. vim.inspect(notes))
      end)
    end)
  end)
end)

T.describe("core.pack.commands :Pack log", function()
  T.it("notifies when there are no log entries", function()
    with_stubbed_command(function(get)
      package.loaded["core.pack.log"] = nil
      package.preload["core.pack.log"] = function() return { list = function() return {} end } end
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        local notes = capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "log" }, bang = false })
          T.eq(ok, true, ":Pack log must not raise: " .. tostring(err))
        end)
        local found
        for _, n in ipairs(notes) do
          if n.msg:match("no log entries") then found = n; break end
        end
        T.truthy(found, "expected 'no log entries' notify, got: " .. vim.inspect(notes))
      end)
      package.preload["core.pack.log"] = nil
      package.loaded["core.pack.log"] = nil
    end)
  end)

  T.it("renders log entries through UI.status", function()
    with_stubbed_command(function(get)
      package.loaded["core.pack.log"] = nil
      package.preload["core.pack.log"] = function()
        return {
          list = function() return {
            { name = "p", from = "abcdef0", to = "1234567", count = 1, subject = "x", ts = os.time() },
          } end,
        }
      end
      local status_called_with
      with_ui_stub({
        status = function(lines, opts) status_called_with = { lines = lines, opts = opts }; return { close = function() end } end,
      }, function()
        fresh_commands().setup(fake_pack())
        capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "log" }, bang = false })
          T.eq(ok, true, ":Pack log must not raise: " .. tostring(err))
        end)
      end)
      package.preload["core.pack.log"] = nil
      package.loaded["core.pack.log"] = nil
      T.truthy(status_called_with, "UI.status should have been invoked")
      T.truthy(status_called_with.lines[1]:match("last %d+ entries"),
        "log header should mention entry count")
    end)
  end)
end)

T.describe("core.pack.commands :Pack sync", function()
  T.it("delegates to update then clean via vim.cmd", function()
    with_stubbed_command(function(get)
      local cmds = {}
      local orig = vim.cmd
      -- vim.cmd can be invoked as a callable or via fields; we override the
      -- callable form which is what the dispatcher uses.
      vim.cmd = function(c) cmds[#cmds + 1] = tostring(c) end
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        local ok, err = pcall(get().callback, { fargs = { "sync" }, bang = false })
        T.eq(ok, true, ":Pack sync must not raise: " .. tostring(err))
      end)
      vim.cmd = orig
      T.eq(cmds, { "Pack update", "Pack clean" })
    end)
  end)

  T.it("sync -y propagates -y to inner update and clean", function()
    with_stubbed_command(function(get)
      local cmds = {}
      local orig = vim.cmd
      vim.cmd = function(c) cmds[#cmds + 1] = tostring(c) end
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        pcall(get().callback, { fargs = { "sync", "-y" }, bang = false })
      end)
      vim.cmd = orig
      T.eq(cmds, { "Pack update -y", "Pack clean -y" })
    end)
  end)

  T.it("sync! propagates bang to inner update and clean", function()
    with_stubbed_command(function(get)
      local cmds = {}
      local orig = vim.cmd
      vim.cmd = function(c) cmds[#cmds + 1] = tostring(c) end
      with_ui_stub({}, function()
        fresh_commands().setup(fake_pack())
        pcall(get().callback, { fargs = { "sync" }, bang = true })
      end)
      vim.cmd = orig
      T.eq(cmds, { "Pack! update", "Pack! clean" })
    end)
  end)
end)

T.describe("core.pack.commands :Pack profile", function()
  T.it("renders profile data through UI.status", function()
    with_stubbed_command(function(get)
      package.loaded["core.profile"] = nil
      package.preload["core.profile"] = function()
        return {
          start             = function() end,
          dump              = function() end,
          lookup            = function() return nil end,
          _structured_report = function()
            return { lines = { "profile header" }, highlights = {} }
          end,
        }
      end
      local rendered
      with_ui_stub({
        status = function(lines, opts) rendered = { lines = lines, opts = opts }; return { close = function() end } end,
      }, function()
        fresh_commands().setup(fake_pack())
        capture_notifies(function()
          local ok, err = pcall(get().callback, { fargs = { "profile" }, bang = false })
          T.eq(ok, true, ":Pack profile must not raise: " .. tostring(err))
        end)
      end)
      package.preload["core.profile"] = nil
      package.loaded["core.profile"] = nil
      T.truthy(rendered, "UI.status should have been called")
      T.eq(rendered.lines[1], "profile header")
      T.eq(rendered.opts.filetype, "PackProfile")
    end)
  end)
end)
