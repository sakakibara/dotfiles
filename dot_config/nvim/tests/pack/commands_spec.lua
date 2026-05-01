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
end)
