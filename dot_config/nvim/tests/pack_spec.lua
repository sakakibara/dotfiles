local T = require("tests.helpers")

local function reset_pack()
  package.loaded["core.pack"] = nil
  package.loaded["core.event"] = nil
  return require("core.pack")
end

T.describe("core.pack validation", function()
  T.it("rejects spec without source", function()
    local pack = reset_pack()
    local ok = pcall(pack.setup, { specs = { { name = "nope" } } })
    T.eq(ok, false)
  end)

  T.it("expands github shorthand into src + name", function()
    local pack = reset_pack()
    pack.setup({ specs = { { "user/plugin-x", lazy = true, event = "BufReadPre" } } })
    T.truthy(pack.has("plugin-x"))
    T.eq(pack.opts("plugin-x"), {})
  end)

  T.it("warns on unknown spec field (does not error)", function()
    local pack = reset_pack()
    local notified = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("unknown field") then notified = true end
    end
    pack.setup({ specs = { { "user/x", nonsense_field = 1, lazy = true, event = "BufReadPre" } } })
    vim.notify = orig
    T.truthy(notified)
  end)
end)

T.describe("core.pack eager loading", function()
  T.it("calls config() for eager specs after packadd", function()
    local pack = reset_pack()
    local called = false
    pack.setup({
      specs = {
        { dev = true, name = "dummy-eager",
          -- no triggers => eager
          config = function() called = true end },
      },
    })
    -- eager specs load during setup
    T.truthy(called)
    T.truthy(pack.loaded("dummy-eager"))
  end)

  T.it("eager specs respect priority order", function()
    local pack = reset_pack()
    local order = {}
    pack.setup({
      specs = {
        { dev = true, name = "low",  priority = 10, config = function() table.insert(order, "low") end },
        { dev = true, name = "high", priority = 100, config = function() table.insert(order, "high") end },
      },
    })
    T.eq(order, { "high", "low" })
  end)

  T.it("on_load fires for already-loaded eager plugin", function()
    local pack = reset_pack()
    local fired = false
    pack.setup({
      specs = { { dev = true, name = "eager-x", config = function() end } },
    })
    pack.on_load("eager-x", function() fired = true end)
    T.truthy(fired)
  end)
end)

T.describe("core.pack lazy triggers", function()
  T.it("event trigger loads plugin on autocmd fire", function()
    local pack = reset_pack()
    local loaded = false
    pack.setup({
      specs = { { dev = true, name = "lazy-evt", event = "User LibTest",
                  config = function() loaded = true end } },
    })
    T.eq(loaded, false)
    vim.api.nvim_exec_autocmds("User", { pattern = "LibTest" })
    T.truthy(loaded)
  end)

  T.it("ft trigger loads plugin on filetype match", function()
    local pack = reset_pack()
    local loaded = false
    pack.setup({
      specs = { { dev = true, name = "lazy-ft", ft = "lua",
                  config = function() loaded = true end } },
    })
    vim.cmd("enew")
    vim.bo.filetype = "lua"
    T.truthy(loaded)
  end)

  T.it("cmd trigger loads plugin on command", function()
    local pack = reset_pack()
    local loaded = false
    pack.setup({
      specs = { { dev = true, name = "lazy-cmd", cmd = "FakeCmd",
                  config = function()
                    loaded = true
                    vim.api.nvim_create_user_command("FakeCmd", function() end, {})
                  end } },
    })
    T.eq(loaded, false)
    vim.cmd("FakeCmd")
    T.truthy(loaded)
  end)
end)

T.describe("core.pack deps + on_load", function()
  T.it("loads dependencies before dependent", function()
    local pack = reset_pack()
    local order = {}
    pack.setup({
      specs = {
        { dev = true, name = "dep-a", config = function() table.insert(order, "a") end },
        { dev = true, name = "dep-b", dependencies = { "dep-a" },
          config = function() table.insert(order, "b") end },
      },
    })
    T.eq(order, { "a", "b" })
  end)

  T.it("warns on unknown dependency", function()
    local pack = reset_pack()
    local warned = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("unknown dependency") then warned = true end
    end
    pack.setup({
      specs = {
        { dev = true, name = "solo", dependencies = { "ghost" }, config = function() end },
      },
    })
    vim.notify = orig
    T.truthy(warned)
  end)

  T.it("on_load registered for a lazy plugin fires on load", function()
    local pack = reset_pack()
    local fired = false
    pack.setup({
      specs = { { dev = true, name = "lazy-p", event = "User XTrigger",
                  config = function() end } },
    })
    pack.on_load("lazy-p", function() fired = true end)
    T.eq(fired, false)
    vim.api.nvim_exec_autocmds("User", { pattern = "XTrigger" })
    T.truthy(fired)
  end)
end)

T.describe("core.pack invariants", function()
  T.it("rejects circular dependencies without stack overflow", function()
    local pack = reset_pack()
    local notified = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.ERROR and msg:match("dependency cycle") then notified = true end
    end
    pack.setup({
      specs = {
        { dev = true, name = "cyc-a", dependencies = { "cyc-b" }, config = function() end },
        { dev = true, name = "cyc-b", dependencies = { "cyc-a" }, config = function() end },
      },
    })
    vim.notify = orig
    T.truthy(notified)
  end)

  T.it("cond = false excludes spec from registration", function()
    local pack = reset_pack()
    local called = false
    pack.setup({
      specs = {
        { dev = true, name = "gated", cond = false, config = function() called = true end },
      },
    })
    T.eq(called, false)
    T.eq(pack.has("gated"), false)
  end)

  T.it("init fires before config for the same spec", function()
    local pack = reset_pack()
    local order = {}
    pack.setup({
      specs = {
        { dev = true, name = "ordered",
          init = function() table.insert(order, "init") end,
          config = function() table.insert(order, "config") end },
      },
    })
    T.eq(order, { "init", "config" })
  end)
end)

T.describe("core.pack re-fires triggering event", function()
  T.it("event trigger re-fires after load so plugin autocmds pick up", function()
    local pack = reset_pack()
    local plugin_saw_event = false
    pack.setup({
      specs = {
        { dev = true, name = "refirer", event = "User RefireTest",
          config = function()
            -- Plugin registers its own autocmd for the SAME event
            vim.api.nvim_create_autocmd("User", {
              once = true, pattern = "RefireTest",
              callback = function() plugin_saw_event = true end,
            })
          end },
      },
    })
    vim.api.nvim_exec_autocmds("User", { pattern = "RefireTest" })
    -- Re-fire is deferred via vim.schedule to break autocmd nesting;
    -- wait briefly for the scheduled callback to run.
    vim.wait(100, function() return plugin_saw_event end)
    T.truthy(plugin_saw_event)
  end)
end)

-- Helper: unmap an lhs across all tested modes, swallowing "no such mapping".
local function unmap_all(lhs, modes)
  for _, m in ipairs(modes or { "n", "x", "v", "o", "i" }) do
    pcall(vim.keymap.del, m, lhs)
  end
end

T.describe("core.pack keys — rhs resolution", function()
  T.it("<Plug> string rhs resolves post-load (eager plugin)", function()
    local pack = reset_pack()
    unmap_all("<F20>")
    pcall(vim.keymap.del, "n", "<Plug>(PackTestPlug)")
    local fired = false
    pack.setup({
      specs = {
        { dev = true, name = "plug-eager",
          keys = { { "<F20>", "<Plug>(PackTestPlug)", mode = "n", desc = "Plug test" } },
          config = function()
            vim.keymap.set("n", "<Plug>(PackTestPlug)",
              function() fired = true end, { silent = true })
          end },
      },
    })
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F20>", true, false, true), "mx", false)
    T.truthy(fired, "<F20> -> <Plug>(PackTestPlug) did not fire")
    unmap_all("<F20>")
    pcall(vim.keymap.del, "n", "<Plug>(PackTestPlug)")
  end)

  T.it("function rhs fires post-load", function()
    local pack = reset_pack()
    unmap_all("<F21>")
    local fired = false
    pack.setup({
      specs = {
        { dev = true, name = "fn-eager",
          keys = { { "<F21>", function() fired = true end, mode = "n", desc = "Fn test" } },
          config = function() end },
      },
    })
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F21>", true, false, true), "mx", false)
    T.truthy(fired)
    unmap_all("<F21>")
  end)

  T.it("<Cmd> string rhs fires post-load", function()
    local pack = reset_pack()
    unmap_all("<F22>")
    pcall(vim.api.nvim_del_user_command, "PackTestCmd")
    local fired = false
    vim.api.nvim_create_user_command("PackTestCmd", function() fired = true end, {})
    pack.setup({
      specs = {
        { dev = true, name = "cmd-eager",
          keys = { { "<F22>", "<Cmd>PackTestCmd<CR>", mode = "n", desc = "Cmd test" } },
          config = function() end },
      },
    })
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F22>", true, false, true), "mx", false)
    T.truthy(fired)
    unmap_all("<F22>")
    pcall(vim.api.nvim_del_user_command, "PackTestCmd")
  end)

  T.it("lazy plugin: keypress loads plugin and fires <Plug> rhs", function()
    local pack = reset_pack()
    unmap_all("<F23>")
    pcall(vim.keymap.del, "n", "<Plug>(PackLazyPlug)")
    local fired = false
    pack.setup({
      specs = {
        { dev = true, name = "plug-lazy",
          event = "User PackLazyTrigger",
          keys = { { "<F23>", "<Plug>(PackLazyPlug)", mode = "n", desc = "Lazy plug" } },
          config = function()
            vim.keymap.set("n", "<Plug>(PackLazyPlug)",
              function() fired = true end, { silent = true })
          end },
      },
    })
    T.eq(pack.loaded("plug-lazy"), false)
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F23>", true, false, true), "mx", false)
    -- replay happens in "m" mode (remap) — give the scheduler a tick
    vim.wait(100, function() return fired end)
    T.truthy(fired, "lazy-triggered <Plug> did not fire after load + replay")
    T.truthy(pack.loaded("plug-lazy"))
    unmap_all("<F23>")
    pcall(vim.keymap.del, "n", "<Plug>(PackLazyPlug)")
  end)

  T.it("ft keys are scoped to matching filetype", function()
    local pack = reset_pack()
    unmap_all("<F24>")
    local fired = false
    pack.setup({
      specs = {
        { dev = true, name = "ft-keys",
          keys = { { "<F24>", function() fired = true end, mode = "n", ft = "lua", desc = "FT test" } },
          config = function() end },
      },
    })
    vim.cmd("enew")
    vim.bo.filetype = "lua"
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F24>", true, false, true), "mx", false)
    T.truthy(fired)
    unmap_all("<F24>")
  end)
end)

T.describe("core.pack keys — <Plug> validation", function()
  T.it("warns when <Plug> rhs has no mapping after load", function()
    local pack = reset_pack()
    unmap_all("<F25>")
    local warned = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("unresolved <Plug>") then warned = true end
    end
    pack.setup({
      specs = {
        { dev = true, name = "bad-plug",
          keys = { { "<F25>", "<Plug>(PackNonExistent)", mode = "n", desc = "Bad" } },
          config = function() end },
      },
    })
    -- bad-plug is lazy (keys-only trigger); force load so install_spec_keys
    -- runs validate_plug. A real user would see this warning the first
    -- time they press the key — at load time, which is the right moment.
    pack.load("bad-plug")
    vim.notify = orig
    T.truthy(warned, "no warning for unresolved <Plug> target")
    unmap_all("<F25>")
  end)
end)

T.describe("core.pack keys — conflict detection", function()
  T.it("warns when two specs bind the same lhs+mode", function()
    local pack = reset_pack()
    unmap_all("<F26>")
    local warned = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("keymap conflict") then warned = true end
    end
    pack.setup({
      specs = {
        { dev = true, name = "a-map",
          keys = { { "<F26>", function() end, mode = "n", desc = "A" } },
          config = function() end },
        { dev = true, name = "b-map",
          keys = { { "<F26>", function() end, mode = "n", desc = "B" } },
          config = function() end },
      },
    })
    vim.notify = orig
    T.truthy(warned, "no warning for duplicate lhs+mode")
    unmap_all("<F26>")
  end)
end)

T.describe("core.pack add_keys API", function()
  T.it("add_keys on loaded plugin installs mapping immediately", function()
    local pack = reset_pack()
    unmap_all("<F27>")
    local fired = false
    pack.setup({
      specs = { { dev = true, name = "addk-loaded", config = function() end } },
    })
    T.truthy(pack.loaded("addk-loaded"))
    pack.add_keys("addk-loaded", {
      { "<F27>", function() fired = true end, mode = "n", desc = "Added" },
    })
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F27>", true, false, true), "mx", false)
    T.truthy(fired)
    unmap_all("<F27>")
  end)

  T.it("add_keys on lazy plugin installs mapping on load", function()
    local pack = reset_pack()
    unmap_all("<F28>")
    local fired = false
    pack.setup({
      specs = { { dev = true, name = "addk-lazy", event = "User AddkLazy",
                  config = function() end } },
    })
    T.eq(pack.loaded("addk-lazy"), false)
    pack.add_keys("addk-lazy", {
      { "<F28>", function() fired = true end, mode = "n", desc = "Added lazy" },
    })
    -- Not yet loaded: map shouldn't be live
    T.eq(vim.fn.maparg("<F28>", "n"), "")
    vim.api.nvim_exec_autocmds("User", { pattern = "AddkLazy" })
    vim.wait(50)
    -- After load: map installed
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F28>", true, false, true), "mx", false)
    T.truthy(fired)
    unmap_all("<F28>")
  end)
end)
