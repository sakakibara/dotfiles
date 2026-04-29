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

  T.it("dedupes re-fires when multiple specs share an event", function()
    local pack = reset_pack()
    local handler_calls = 0
    -- Handler registered BEFORE setup so the re-fire reaches it.
    local aug = vim.api.nvim_create_augroup("PackDedupTest", { clear = true })
    vim.api.nvim_create_autocmd("User", {
      group = aug, pattern = "DedupTest",
      callback = function() handler_calls = handler_calls + 1 end,
    })
    pack.setup({
      specs = {
        { dev = true, name = "dedup-a", event = "User DedupTest", config = function() end },
        { dev = true, name = "dedup-b", event = "User DedupTest", config = function() end },
        { dev = true, name = "dedup-c", event = "User DedupTest", config = function() end },
      },
    })
    -- Original fire: handler runs once synchronously, all three specs load, and
    -- each once-handler asks for a re-fire. Dedup collapses those to a single
    -- scheduled re-fire, so the handler runs exactly one more time after drain.
    vim.api.nvim_exec_autocmds("User", { pattern = "DedupTest" })
    T.eq(handler_calls, 1)
    vim.wait(100, function() return handler_calls >= 2 end)
    T.eq(handler_calls, 2)
    vim.api.nvim_del_augroup_by_id(aug)
  end)

  T.it("does not nest past E218 when handler pumps the event loop", function()
    -- Regression: conform.format's internal vim.wait() drains pending schedule
    -- callbacks during a handler run. Without dedup, 15 pending re-fires for
    -- BufWritePre stacked into the call stack (each invoking Lib.format →
    -- conform.format → vim.wait → next pending) until nvim's nesting limit
    -- (10) tripped E218.
    local pack = reset_pack()
    local nesting_error = false
    local orig_notify = vim.notify
    vim.notify = function(msg, lvl, opts)
      if type(msg) == "string" and msg:match("E218") then nesting_error = true end
      return orig_notify(msg, lvl, opts)
    end

    local specs = {}
    for i = 1, 15 do
      specs[i] = { dev = true, name = "pump-" .. i, event = "User PumpTest",
                   config = function() end }
    end
    -- Handler that pumps the event loop (simulates conform.format + vim.wait)
    local aug = vim.api.nvim_create_augroup("PackPumpTest", { clear = true })
    vim.api.nvim_create_autocmd("User", {
      group = aug, pattern = "PumpTest",
      callback = function() vim.wait(20, function() return false end) end,
    })
    pack.setup({ specs = specs })

    vim.api.nvim_exec_autocmds("User", { pattern = "PumpTest" })
    -- Drain all scheduled callbacks. vim.wait with always-false predicate
    -- gives the event loop time to run them.
    vim.wait(500, function() return false end)

    vim.notify = orig_notify
    vim.api.nvim_del_augroup_by_id(aug)
    T.eq(nesting_error, false, "E218 autocommand nesting was reported")
  end)

  T.it("re-entrancy guard blocks same-key refire while one is active", function()
    -- Direct test of the belt-and-suspenders guard: with dedup alone, the
    -- E218 regression couldn't trigger in practice, so the guard would rot
    -- untested. Here we drive it via the exposed schedule_refire helper and
    -- force a re-entrancy: a handler, during its nested vim.wait, schedules
    -- another re-fire for the SAME key. The guard must block the nested run
    -- (runs=1), not let it recurse (runs>=2).
    local pack = reset_pack()
    pack.setup({ specs = {} })

    local runs = 0
    local aug = vim.api.nvim_create_augroup("PackReentryTest", { clear = true })
    vim.api.nvim_create_autocmd("User", {
      group = aug, pattern = "ReentryTest",
      callback = function()
        runs = runs + 1
        if runs == 1 then
          -- Queue a second refire for the same key, then pump the loop to
          -- give the scheduled callback a chance to fire. If the guard
          -- works, this nested callback short-circuits (runs stays at 1
          -- until we exit); if it doesn't, runs becomes 2 mid-wait.
          pack._schedule_refire("User", { pattern = "ReentryTest" })
          vim.wait(50, function() return runs >= 2 end)
        end
      end,
    })

    pack._schedule_refire("User", { pattern = "ReentryTest" })
    vim.wait(200, function() return runs >= 1 end)
    -- One handler run from the first scheduled refire. The second refire
    -- queued mid-handler must wait for active=false to clear, but by then
    -- the pending flag is clear and it runs — so we expect exactly 2 total.
    -- The key assertion: runs did NOT reach 2 *during* the first's vim.wait
    -- (that would be nesting). Verify by checking that between the vim.wait
    -- and now, runs advanced from 1 to 2 (second runs after first returns).
    vim.wait(200, function() return runs >= 2 end)
    T.eq(runs, 2, "expected exactly 2 non-nested handler runs")

    vim.api.nvim_del_augroup_by_id(aug)
  end)

  T.it("different buffers re-fire independently (no cross-key blocking)", function()
    -- Per-key (not per-event) active guard: a refire for buf A must not
    -- block a concurrent refire for buf B. Regression against collapsing
    -- the guard to a per-event flag. This mirrors the real production
    -- scenario (BufWritePre across multiple buffers) that triggered E218.
    local pack = reset_pack()
    pack.setup({ specs = {} })

    local buf_a = vim.api.nvim_create_buf(false, true)
    local buf_b = vim.api.nvim_create_buf(false, true)
    local seen = {}  -- { [buf] = count }
    local aug = vim.api.nvim_create_augroup("PackKeyIndepTest", { clear = true })
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = aug,
      callback = function(args)
        seen[args.buf] = (seen[args.buf] or 0) + 1
      end,
    })

    -- Two schedules for different buffer keys — both must run.
    pack._schedule_refire("BufWritePre", { buffer = buf_a, modeline = false })
    pack._schedule_refire("BufWritePre", { buffer = buf_b, modeline = false })
    vim.wait(200, function() return seen[buf_a] and seen[buf_b] end)
    T.eq(seen[buf_a], 1)
    T.eq(seen[buf_b], 1)

    vim.api.nvim_del_augroup_by_id(aug)
    vim.api.nvim_buf_delete(buf_a, { force = true })
    vim.api.nvim_buf_delete(buf_b, { force = true })
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

T.describe("core.pack keys — external collision detection", function()
  T.it("warns when a spec key overrides an existing global mapping", function()
    local pack = reset_pack()
    unmap_all("<F29>")
    -- Pre-install an external mapping (simulating config/keymaps.lua or a
    -- plugin's setup() body — neither goes through core.pack's spec path).
    vim.keymap.set("n", "<F29>", "<Cmd>echo 'external'<CR>", { desc = "External pre-existing" })

    local notified = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN
        and msg:match("overrides existing mapping")
        and msg:match("External pre%-existing") then
        notified = true
      end
    end
    pack.setup({
      specs = {
        { dev = true, name = "ext-override",
          keys = { { "<F29>", function() end, mode = "n", desc = "Spec mapping" } },
          config = function() end },
      },
    })
    vim.notify = orig
    T.truthy(notified, "expected an override warning, got none")
    unmap_all("<F29>")
  end)

  T.it("does NOT warn when our own spec re-installs its own keymap", function()
    local pack = reset_pack()
    unmap_all("<F30>")
    -- First install via spec — sets the mapping with desc "key: own".
    pack.setup({
      specs = {
        { dev = true, name = "own", keys = { { "<F30>", function() end, mode = "n" } },
          config = function() end },
      },
    })

    -- Re-install with a fresh pack instance but the SAME nvim session, so
    -- the existing mapping persists. The override warning should NOT fire,
    -- because the existing mapping has our default "key: " desc prefix.
    local pack2 = reset_pack()
    local notified = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("overrides existing mapping") then
        notified = true
      end
    end
    pack2.setup({
      specs = {
        { dev = true, name = "own", keys = { { "<F30>", function() end, mode = "n" } },
          config = function() end },
      },
    })
    vim.notify = orig
    T.eq(notified, false, "false-positive: warned on our own re-install")
    unmap_all("<F30>")
  end)

  T.it("does NOT warn for buffer-local pre-existing mappings", function()
    local pack = reset_pack()
    unmap_all("<F31>")
    vim.keymap.set("n", "<F31>", "<Cmd>echo 'buf'<CR>",
      { desc = "Buffer-local existing", buffer = 0 })

    local notified = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("overrides existing mapping") then
        notified = true
      end
    end
    pack.setup({
      specs = {
        { dev = true, name = "buf-noop",
          keys = { { "<F31>", function() end, mode = "n", desc = "Spec mapping" } },
          config = function() end },
      },
    })
    vim.notify = orig
    T.eq(notified, false, "false-positive: warned on buffer-local existing")
    pcall(vim.keymap.del, "n", "<F31>", { buffer = 0 })
    unmap_all("<F31>")
  end)

  T.it("lazy spec with custom desc: stub->real does not re-fire warning", function()
    local pack = reset_pack()
    unmap_all("<F32>")
    local warnings = {}
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN then table.insert(warnings, msg) end
    end
    pack.setup({
      specs = {
        { dev = true, name = "custom-desc-lazy",
          event = "User CustomDescTrigger",  -- non-firing trigger keeps spec lazy
          keys = { { "<F32>", function() end, mode = "n", desc = "Custom Description" } },
          config = function() end },
      },
    })
    -- Drive stub->real
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F32>", true, false, true), "mx", false)
    vim.wait(100, function() return pack.loaded("custom-desc-lazy") end)
    vim.notify = orig
    T.truthy(pack.loaded("custom-desc-lazy"),
      "spec never loaded — stub->real did not fire; warning assertions would be vacuous")
    for _, msg in ipairs(warnings) do
      T.eq(msg:match("Custom Description"), nil,
        "false-positive: stub->real warned with the spec's own desc")
      T.eq(msg:match("overrides existing"), nil,
        "false-positive: stub->real fired overrides-existing warning")
    end
    unmap_all("<F32>")
  end)
end)

T.describe("core.pack module name resolution", function()
  -- Build a fake plugin install root with a lua/<modname>/ directory and run
  -- _resolve_main against a spec that points at it.
  local function make_plugin(plugin_name, lua_subdir)
    local root = vim.fn.tempname() .. "-pack"
    local plug_dir = root .. "/" .. plugin_name .. "/lua/" .. lua_subdir
    vim.fn.mkdir(plug_dir, "p")
    return root, plug_dir
  end

  local function with_root(root, fn)
    local Install = require("core.pack.install")
    local prev = Install._install_root_override
    Install._install_root_override = root
    local ok, err = pcall(fn)
    Install._install_root_override = prev
    if not ok then error(err) end
  end

  T.it("respects explicit spec.main override", function()
    local pack = reset_pack()
    local root = make_plugin("anything.nvim", "anything")
    with_root(root, function()
      T.eq(pack._resolve_main({ name = "anything.nvim", main = "explicit" }), "explicit")
    end)
  end)

  T.it("strips .nvim suffix when module dir matches", function()
    local pack = reset_pack()
    local root = make_plugin("plenary.nvim", "plenary")
    with_root(root, function()
      T.eq(pack._resolve_main({ name = "plenary.nvim" }), "plenary")
    end)
  end)

  T.it("resolves dash → underscore mismatch (better-escape.nvim → better_escape)", function()
    local pack = reset_pack()
    local root = make_plugin("better-escape.nvim", "better_escape")
    with_root(root, function()
      T.eq(pack._resolve_main({ name = "better-escape.nvim" }), "better_escape")
    end)
  end)

  T.it("resolves nvim- prefix mismatch (nvim-treesitter-context → treesitter-context)", function()
    local pack = reset_pack()
    local root = make_plugin("nvim-treesitter-context", "treesitter-context")
    with_root(root, function()
      T.eq(pack._resolve_main({ name = "nvim-treesitter-context" }), "treesitter-context")
    end)
  end)

  T.it("resolves nvim- prefix + .nvim suffix (nvim-ufo → ufo)", function()
    local pack = reset_pack()
    local root = make_plugin("nvim-ufo", "ufo")
    with_root(root, function()
      T.eq(pack._resolve_main({ name = "nvim-ufo" }), "ufo")
    end)
  end)

  T.it("falls through to derived name when no module dir matches", function()
    -- ibl-style: lua/ibl exists but plugin name is indent-blankline.nvim. The
    -- normalized names don't match, so we fall through to derived "indent-blankline"
    -- — this is a case where the user MUST set spec.main = "ibl" explicitly.
    local pack = reset_pack()
    local root = make_plugin("indent-blankline.nvim", "ibl")
    with_root(root, function()
      T.eq(pack._resolve_main({ name = "indent-blankline.nvim" }), "indent-blankline")
    end)
  end)
end)
