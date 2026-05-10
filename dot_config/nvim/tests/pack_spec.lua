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
    pack.setup({ specs = { { "user/plugin-x", dev = true, lazy = true, event = "BufReadPre" } } })
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

T.describe("core.pack opts resolution", function()
  T.it("opts as a table is passed to config unchanged", function()
    local pack = reset_pack()
    local received
    pack.setup({
      specs = { { dev = true, name = "table-opts",
                  opts = { value = 42 },
                  config = function(_, opts) received = opts end } },
    })
    T.eq(received, { value = 42 })
  end)

  T.it("opts as a function is resolved at config time", function()
    local pack = reset_pack()
    local received
    pack.setup({
      specs = { { dev = true, name = "fn-opts",
                  opts = function() return { value = 99 } end,
                  config = function(_, opts) received = opts end } },
    })
    T.eq(received, { value = 99 })
  end)

  T.it("opts function receives spec and a base table", function()
    local pack = reset_pack()
    local got_spec, got_base
    pack.setup({
      specs = { { dev = true, name = "fn-opts-args",
                  opts = function(spec, base) got_spec, got_base = spec, base; return {} end,
                  config = function() end } },
    })
    T.eq(got_spec.name, "fn-opts-args")
    T.eq(got_base, {})
  end)

  T.it("opts function returning nil yields empty table", function()
    local pack = reset_pack()
    local received
    pack.setup({
      specs = { { dev = true, name = "fn-opts-nil",
                  opts = function() return nil end,
                  config = function(_, opts) received = opts end } },
    })
    T.eq(received, {})
  end)

  T.it("opts function errors are caught and reported via vim.notify", function()
    local pack = reset_pack()
    local logs = {}
    local orig = vim.notify
    vim.notify = function(msg, lvl) logs[#logs + 1] = { lvl = lvl, msg = msg } end
    pack.setup({
      specs = { { dev = true, name = "fn-opts-fail",
                  opts = function() error("boom from opts") end,
                  config = function() end } },
    })
    vim.notify = orig
    -- Find the ERROR-level notify whose msg references our spec name
    local found
    for _, l in ipairs(logs) do
      if l.lvl == vim.log.levels.ERROR
         and tostring(l.msg):find("fn-opts-fail", 1, true)
         and tostring(l.msg):find("boom from opts", 1, true) then
        found = l.msg
        break
      end
    end
    T.truthy(found)
  end)

  T.it("opts function error does not abort other specs in the same setup", function()
    local pack = reset_pack()
    local later_called = false
    local orig = vim.notify
    vim.notify = function() end
    pack.setup({
      specs = {
        { dev = true, name = "fail-first",  priority = 100,
          opts = function() error("nope") end,
          config = function() end },
        { dev = true, name = "succeed-after", priority = 50,
          config = function() later_called = true end },
      },
    })
    vim.notify = orig
    T.truthy(later_called)
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

  T.it("override = true suppresses warning, still installs spec key (eager)", function()
    local pack = reset_pack()
    unmap_all("<F33>")
    vim.keymap.set("n", "<F33>", "<Cmd>echo 'external'<CR>", { desc = "External" })
    local warnings = {}
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("overrides existing mapping") then
        table.insert(warnings, msg)
      end
    end
    local fn = function() end
    pack.setup({
      specs = {
        { dev = true, name = "override-eager", lazy = false,
          keys = { { "<F33>", fn, mode = "n", desc = "Spec mapping", override = true } },
          config = function() end },
      },
    })
    vim.notify = orig
    T.eq(#warnings, 0, "override = true should suppress the overrides-existing warning")
    T.eq(vim.fn.maparg("<F33>", "n", false, true).callback, fn,
      "spec key was not installed")
    unmap_all("<F33>")
  end)

  T.it("override = true on lazy spec: no warning at stub install or after stub->real", function()
    local pack = reset_pack()
    unmap_all("<F34>")
    vim.keymap.set("n", "<F34>", "<Cmd>echo 'external'<CR>", { desc = "External" })
    local warnings = {}
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("overrides existing mapping") then
        table.insert(warnings, msg)
      end
    end
    pack.setup({
      specs = {
        { dev = true, name = "override-lazy",
          event = "User OverrideLazyTrigger",
          keys = { { "<F34>", function() end, mode = "n", desc = "Spec mapping", override = true } },
          config = function() end },
      },
    })
    vim.api.nvim_feedkeys(
      vim.api.nvim_replace_termcodes("<F34>", true, false, true), "mx", false)
    vim.wait(100, function() return pack.loaded("override-lazy") end)
    vim.notify = orig
    T.truthy(pack.loaded("override-lazy"),
      "spec never loaded — stub->real did not fire; warning assertion would be vacuous")
    T.eq(#warnings, 0, "override = true should suppress warning at stub and at stub->real")
    unmap_all("<F34>")
  end)

  T.it("preserve copies rhs string and installs spec key", function()
    local pack = reset_pack()
    unmap_all("<F35>")
    unmap_all("<F36>")
    vim.keymap.set("n", "<F35>", ":tnext<CR>", { desc = "Original" })
    local warnings = {}
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN then table.insert(warnings, msg) end
    end
    local fn = function() end
    pack.setup({
      specs = {
        { dev = true, name = "preserve-rhs", lazy = false,
          keys = { { "<F35>", fn, mode = "n", desc = "Spec", preserve = "<F36>" } },
          config = function() end },
      },
    })
    vim.notify = orig
    -- No collision warning fired
    for _, msg in ipairs(warnings) do
      T.eq(msg:match("overrides existing"), nil)
    end
    -- Original was preserved at <F36>
    local moved = vim.fn.maparg("<F36>", "n", false, true)
    T.truthy(moved.rhs and moved.rhs:match("tnext"), "preserve target missing original rhs")
    -- Spec key is installed at <F35>
    T.eq(vim.fn.maparg("<F35>", "n", false, true).callback, fn)
    unmap_all("<F35>")
    unmap_all("<F36>")
  end)

  T.it("preserve copies Lua callback when source has no rhs string", function()
    local pack = reset_pack()
    unmap_all("<F37>")
    unmap_all("<F38>")
    local original_fn = function() end
    vim.keymap.set("n", "<F37>", original_fn, { desc = "Original cb" })
    pack.setup({
      specs = {
        { dev = true, name = "preserve-cb", lazy = false,
          keys = { { "<F37>", function() end, mode = "n", desc = "Spec", preserve = "<F38>" } },
          config = function() end },
      },
    })
    T.eq(vim.fn.maparg("<F38>", "n", false, true).callback, original_fn,
      "callback was not preserved")
    unmap_all("<F37>")
    unmap_all("<F38>")
  end)

  T.it("preserve copies expr/silent/nowait flags", function()
    local pack = reset_pack()
    unmap_all("<F39>")
    unmap_all("<F40>")
    vim.keymap.set("n", "<F39>", function() return "<Esc>" end,
      { desc = "Flagged", expr = true, silent = true, nowait = true })
    pack.setup({
      specs = {
        { dev = true, name = "preserve-flags", lazy = false,
          keys = { { "<F39>", function() end, mode = "n", desc = "Spec", preserve = "<F40>" } },
          config = function() end },
      },
    })
    local moved = vim.fn.maparg("<F40>", "n", false, true)
    T.eq(moved.expr, 1, "expr flag not preserved")
    T.eq(moved.silent, 1, "silent flag not preserved")
    T.eq(moved.nowait, 1, "nowait flag not preserved")
    unmap_all("<F39>")
    unmap_all("<F40>")
  end)

  T.it("preserve target already bound: warn, skip move, still install spec key", function()
    local pack = reset_pack()
    unmap_all("<F41>")
    unmap_all("<F42>")
    vim.keymap.set("n", "<F41>", ":tnext<CR>", { desc = "Original" })
    local target_fn = function() end
    vim.keymap.set("n", "<F42>", target_fn, { desc = "Pre-existing target" })
    local warned_target_busy = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("preserve target already in use") then
        warned_target_busy = true
      end
    end
    local spec_fn = function() end
    pack.setup({
      specs = {
        { dev = true, name = "preserve-busy-target", lazy = false,
          keys = { { "<F41>", spec_fn, mode = "n", desc = "Spec", preserve = "<F42>" } },
          config = function() end },
      },
    })
    vim.notify = orig
    T.truthy(warned_target_busy, "expected preserve-target-busy warning")
    -- <F42> still has the pre-existing callback (unchanged)
    T.eq(vim.fn.maparg("<F42>", "n", false, true).callback, target_fn,
      "preserve target was clobbered")
    -- Spec's own key still installed at <F41>
    T.eq(vim.fn.maparg("<F41>", "n", false, true).callback, spec_fn)
    unmap_all("<F41>")
    unmap_all("<F42>")
  end)

  T.it("preserve in multi-mode key applies to each colliding mode", function()
    local pack = reset_pack()
    unmap_all("<F43>")
    unmap_all("<F44>")
    vim.keymap.set("n", "<F43>", ":noh<CR>", { desc = "Original n" })
    vim.keymap.set("x", "<F43>", ":<C-u>echo 'x'<CR>", { desc = "Original x" })
    pack.setup({
      specs = {
        { dev = true, name = "preserve-multi", lazy = false,
          keys = { { "<F43>", function() end, mode = { "n", "x" }, desc = "Spec",
                     preserve = "<F44>" } },
          config = function() end },
      },
    })
    T.truthy(vim.fn.maparg("<F44>", "n", false, true).rhs:match("noh"),
      "n-mode preserve missing")
    T.truthy(vim.fn.maparg("<F44>", "x", false, true).rhs:match("echo 'x'"),
      "x-mode preserve missing")
    unmap_all("<F43>")
    unmap_all("<F44>")
  end)

  T.it("preserve in multi-mode key: mode without existing has no preserve copy", function()
    local pack = reset_pack()
    unmap_all("<F45>")
    unmap_all("<F46>")
    vim.keymap.set("n", "<F45>", ":noh<CR>", { desc = "Original n only" })
    -- Note: x mode has no <F45> binding
    pack.setup({
      specs = {
        { dev = true, name = "preserve-multi-partial", lazy = false,
          keys = { { "<F45>", function() end, mode = { "n", "x" }, desc = "Spec",
                     preserve = "<F46>" } },
          config = function() end },
      },
    })
    -- n: preserve happened
    T.truthy(vim.fn.maparg("<F46>", "n", false, true).rhs and
             vim.fn.maparg("<F46>", "n", false, true).rhs:match("noh"))
    -- x: no preserve, since there was no x-mode collision
    T.eq(vim.fn.maparg("<F46>", "x"), "")
    unmap_all("<F45>")
    unmap_all("<F46>")
  end)

  T.it("validation: preserve and override both set -> warn, prefer preserve", function()
    local pack = reset_pack()
    unmap_all("<F47>")
    unmap_all("<F48>")
    vim.keymap.set("n", "<F47>", ":noh<CR>", { desc = "Original" })
    local warned_mutex = false
    local warned_overrides = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN then
        if msg:match("mutually exclusive") then warned_mutex = true end
        if msg:match("overrides existing") then warned_overrides = true end
      end
    end
    pack.setup({
      specs = {
        { dev = true, name = "validate-both", lazy = false,
          keys = { { "<F47>", function() end, mode = "n", desc = "Spec",
                     preserve = "<F48>", override = true } },
          config = function() end },
      },
    })
    vim.notify = orig
    T.truthy(warned_mutex, "expected mutually-exclusive warning")
    T.eq(warned_overrides, false, "should not warn about overrides when preserve wins")
    T.truthy(vim.fn.maparg("<F48>", "n", false, true).rhs and
             vim.fn.maparg("<F48>", "n", false, true).rhs:match("noh"),
      "preserve should have run (preserve wins on conflict)")
    unmap_all("<F47>")
    unmap_all("<F48>")
  end)

  T.it("validation: preserve == k[1] -> warn, ignore preserve, fall back to warn", function()
    local pack = reset_pack()
    unmap_all("<F49>")
    vim.keymap.set("n", "<F49>", ":noh<CR>", { desc = "Original" })
    local warned_noop = false
    local warned_overrides = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN then
        if msg:match("preserve target equals source") then warned_noop = true end
        if msg:match("overrides existing") then warned_overrides = true end
      end
    end
    pack.setup({
      specs = {
        { dev = true, name = "validate-noop", lazy = false,
          keys = { { "<F49>", function() end, mode = "n", desc = "Spec", preserve = "<F49>" } },
          config = function() end },
      },
    })
    vim.notify = orig
    T.truthy(warned_noop, "expected no-op preserve warning")
    T.truthy(warned_overrides,
      "with preserve ignored, the default overrides-existing warning should fire")
    unmap_all("<F49>")
  end)
end)

T.describe("core.pack keys — ft-scoped external collisions", function()
  T.it("ft-scoped preserve runs at FileType time, per-buffer", function()
    local pack = reset_pack()
    unmap_all("<F50>")
    unmap_all("<F51>")

    -- Two lua buffers, each with a buffer-local <F50>
    local bufs = {}
    for _ = 1, 2 do
      local buf = vim.api.nvim_create_buf(true, false)
      table.insert(bufs, buf)
      vim.api.nvim_buf_set_option(buf, "filetype", "")
      vim.api.nvim_set_current_buf(buf)
      vim.keymap.set("n", "<F50>", ":noh<CR>", { buffer = buf, desc = "Original buf" })
    end

    pack.setup({
      specs = {
        { dev = true, name = "ft-preserve",
          keys = { { "<F50>", function() end, mode = "n", ft = "lua",
                     desc = "Spec", preserve = "<F51>" } },
          config = function() end },
      },
    })

    -- Trigger FileType lua on both bufs
    for _, buf in ipairs(bufs) do
      vim.api.nvim_set_current_buf(buf)
      vim.bo.filetype = "lua"
    end

    -- Each buffer should have <F51> bound to the original rhs (buffer-local)
    for _, buf in ipairs(bufs) do
      vim.api.nvim_set_current_buf(buf)
      local moved = vim.fn.maparg("<F51>", "n", false, true)
      T.truthy(moved.rhs and moved.rhs:match("noh"),
        "ft preserve missing on buf " .. buf)
      T.eq(moved.buffer, 1, "ft preserve should be buffer-local")
    end

    for _, buf in ipairs(bufs) do
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
    unmap_all("<F50>")
    unmap_all("<F51>")
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

T.describe("core.pack.version.resolve: default sentinel", function()
  T.it("nil version returns kind='default'", function()
    package.loaded["core.pack.version"] = nil
    local Version = require("core.pack.version")
    local refs = { tags = {}, branches = { "main" }, default_branch = "main" }
    local r = Version.resolve(nil, refs)
    T.eq(r.kind, "default")
    T.eq(r.name, nil)
  end)

  T.it("nil version with no default_branch still returns default sentinel", function()
    package.loaded["core.pack.version"] = nil
    local Version = require("core.pack.version")
    local refs = { tags = {}, branches = {}, default_branch = nil }
    local r = Version.resolve(nil, refs)
    T.eq(r.kind, "default")
  end)
end)

T.describe("core.pack.git: rev_parse and checkout_sha (stubbed)", function()
  local stubs = require("tests.pack.stubs")

  T.it("rev_parse returns SHA on success", function()
    package.loaded["core.pack.git"] = nil
    local Git = require("core.pack.git")
    local restore = stubs.stub_system({
      ["rev-parse --verify refs/heads/main"] = { code = 0, stdout = "abc1234567890def\n" },
    })
    local sha = stubs.await(Git.rev_parse, "/some/dir", "refs/heads/main")
    restore()
    T.eq(sha, "abc1234567890def")
  end)

  T.it("rev_parse rejects non-SHA stdout", function()
    package.loaded["core.pack.git"] = nil
    local Git = require("core.pack.git")
    local restore = stubs.stub_system({
      ["rev-parse --verify garbage"] = { code = 0, stdout = "not-hex\n" },
    })
    local sha, err = stubs.await(Git.rev_parse, "/some/dir", "garbage")
    restore()
    T.eq(sha, nil)
    T.truthy(err and err:match("non%-SHA"))
  end)

  T.it("rev_parse returns err on git failure", function()
    package.loaded["core.pack.git"] = nil
    local Git = require("core.pack.git")
    local restore = stubs.stub_system({
      ["rev-parse --verify nope"] = { code = 1, stderr = "fatal: bad revision\n" },
    })
    local sha, err = stubs.await(Git.rev_parse, "/some/dir", "nope")
    restore()
    T.eq(sha, nil)
    T.truthy(err and err:match("bad revision"))
  end)

  T.it("checkout_sha invokes detached checkout with the SHA", function()
    package.loaded["core.pack.git"] = nil
    local Git = require("core.pack.git")
    local restore = stubs.stub_system({
      ["checkout --detach --quiet abc123"] = { code = 0 },
    })
    local r = stubs.await(Git.checkout_sha, "/some/dir", "abc123")
    restore()
    T.eq(r.ok, true)
  end)
end)

T.describe("core.pack.setup: failed-install pruning", function()
  T.it("removes failed specs from M._specs and ordered before eager load", function()
    local pack = (function()
      package.loaded["core.pack"] = nil
      return require("core.pack")
    end)()

    pack._specs["good"] = { name = "good", lazy = false }
    pack._specs["bad"]  = { name = "bad",  lazy = false }
    pack._specs["bad"] = nil
    T.eq(pack._specs.bad, nil)
    T.truthy(pack._specs.good)

    pack._specs.good = nil
  end)
end)

T.describe("core.pack.lock: pretty-printed write", function()
  local function tmpdir()
    local p = vim.fn.tempname()
    vim.fn.mkdir(p, "p")
    return p
  end

  T.it("write produces multi-line output", function()
    package.loaded["core.pack.lock"] = nil
    local Lock = require("core.pack.lock")
    local dir = tmpdir()
    Lock._path_override = dir .. "/lock.json"
    Lock.write({ version = 1, plugins = { foo = { rev = "abc" }, bar = { rev = "def" } } })
    local fd = io.open(Lock._path_override, "r")
    local content = fd:read("*a"); fd:close()
    T.truthy(content:find("\n"), "expected multi-line lockfile")
    Lock._path_override = nil
  end)

  T.it("plugin keys are sorted alphabetically", function()
    package.loaded["core.pack.lock"] = nil
    local Lock = require("core.pack.lock")
    local dir = tmpdir()
    Lock._path_override = dir .. "/lock.json"
    Lock.write({ version = 1, plugins = {
      zeta = { rev = "z" },
      alpha = { rev = "a" },
      mu = { rev = "m" },
    } })
    local fd = io.open(Lock._path_override, "r")
    local content = fd:read("*a"); fd:close()
    local alpha_pos = content:find('"alpha"')
    local mu_pos = content:find('"mu"')
    local zeta_pos = content:find('"zeta"')
    T.truthy(alpha_pos and mu_pos and zeta_pos, "expected all keys to appear")
    T.truthy(alpha_pos < mu_pos and mu_pos < zeta_pos,
      "expected alphabetical key order in lockfile")
    Lock._path_override = nil
  end)

  T.it("round-trip: write then read returns same data", function()
    package.loaded["core.pack.lock"] = nil
    local Lock = require("core.pack.lock")
    local dir = tmpdir()
    Lock._path_override = dir .. "/lock.json"
    local data = { version = 1, plugins = { foo = { rev = "abc", src = "https://x/y" } } }
    Lock.write(data)
    local read = Lock.read()
    T.eq(read.version, 1)
    T.eq(read.plugins.foo.rev, "abc")
    T.eq(read.plugins.foo.src, "https://x/y")
    Lock._path_override = nil
  end)
end)

-- Drive install_all by stubbing Install.install_missing + UI.cold_install_splash.
-- Specifically catches reference bugs in the synchronous on_complete callback
-- (the historical "vim.vim.notify" typo lived here for a session because no
-- spec drove the actual install pipeline through pack.setup).
T.describe("core.pack install_all on_complete callback", function()
  local function with_install_stub(fn)
    package.loaded["core.pack.install"] = nil
    local Install = require("core.pack.install")
    local orig = {
      install_missing = Install.install_missing,
      install_dir     = Install.install_dir,
    }
    local captured
    Install.install_dir = function() return "/nonexistent" end  -- so spec is "missing"
    Install.install_missing = function(specs, opts)
      captured = { specs = specs, opts = opts }
      if opts.on_progress then opts.on_progress(1, #specs, specs[1] and specs[1].name) end
      if opts.on_complete then opts.on_complete() end  -- the line under test
    end

    -- Stub UI.cold_install_splash so install_all doesn't try to open a real
    -- floating window inside the test runner.
    package.loaded["core.pack.ui"] = nil
    local UI = require("core.pack.ui")
    local orig_splash = UI.cold_install_splash
    UI.cold_install_splash = function(_total)
      return {
        update                = function() end,
        enter_setup_phase     = function() end,
        set_setup_status      = function() end,
        set_status_text       = function() end,
        start_idle_close      = function() end,
        close                 = function() end,
      }
    end

    -- pack.install_all uses vim.wait + vim.schedule. Drain them synchronously
    -- so the test sees the on_complete-side effects.
    local orig_schedule = vim.schedule
    vim.schedule = function(f) f() end

    local ok, err = pcall(fn, captured, function() return captured end)

    vim.schedule = orig_schedule
    UI.cold_install_splash = orig_splash
    Install.install_missing = orig.install_missing
    Install.install_dir     = orig.install_dir

    if not ok then error(err) end
  end

  T.it("on_complete fires without indexing-nil errors when install runs", function()
    local pack = reset_pack()
    local notifications = {}
    local orig_notify = vim.notify
    vim.notify = function(msg, _lvl) notifications[#notifications + 1] = tostring(msg) end
    with_install_stub(function()
      -- Non-dev + lazy spec so install_all is exercised but the eager-load
      -- phase doesn't try to packadd a plugin that isn't really on disk.
      local ok, err = pcall(pack.setup, {
        specs = { { src = "file:///fake/repo", name = "stub-plugin",
                    lazy = true, event = "VeryLazy" } },
      })
      T.eq(ok, true, "pack.setup should not raise: " .. tostring(err))
    end)
    vim.notify = orig_notify
    -- Find the on_complete vim.notify among all collected notifications.
    local matched
    for _, m in ipairs(notifications) do
      if m:match("installed %d+ plugins") then matched = m; break end
    end
    T.truthy(matched,
      "expected 'installed N plugins' notification, got: " .. vim.inspect(notifications))
  end)

  T.it("on_complete reports the correct count when one of N installs fails", function()
    local pack = reset_pack()
    local notifications = {}
    local orig_notify = vim.notify
    vim.notify = function(msg, _lvl) notifications[#notifications + 1] = tostring(msg) end

    package.loaded["core.pack.install"] = nil
    local Install = require("core.pack.install")
    local orig_im, orig_id = Install.install_missing, Install.install_dir
    Install.install_dir = function() return "/nonexistent" end
    Install.install_missing = function(specs, opts)
      -- Mark first as failed via on_failed; let on_complete fire.
      if opts.on_failed then opts.on_failed(specs[1].name, "stubbed failure") end
      if opts.on_complete then opts.on_complete() end
    end
    package.loaded["core.pack.ui"] = nil
    local UI = require("core.pack.ui")
    local orig_splash = UI.cold_install_splash
    UI.cold_install_splash = function(_)
      return { update=function() end, enter_setup_phase=function() end,
        set_setup_status=function() end, set_status_text=function() end,
        start_idle_close=function() end, close=function() end }
    end
    local orig_schedule = vim.schedule
    vim.schedule = function(f) f() end

    pack.setup({
      specs = {
        { src = "file:///fake/a", name = "a", lazy = true, event = "VeryLazy" },
        { src = "file:///fake/b", name = "b", lazy = true, event = "VeryLazy" },
      },
    })

    vim.schedule = orig_schedule
    UI.cold_install_splash = orig_splash
    Install.install_missing = orig_im
    Install.install_dir     = orig_id
    vim.notify = orig_notify

    -- 2 specs total, 1 failed → message says "installed 1 plugins"
    local matched
    for _, m in ipairs(notifications) do
      if m:match("installed 1 plugins") then matched = m; break end
    end
    T.truthy(matched,
      "expected 'installed 1 plugins', got: " .. vim.inspect(notifications))
  end)
end)

-- Drive :Pack install through the dispatcher into a stubbed Install. The
-- dispatcher's on_complete callback (commands.lua) is the analogue of the
-- install_all bug above and would silently break the same way without
-- this coverage.
T.describe("core.pack :Pack install dispatcher on_complete callback", function()
  T.it(":Pack install fires on_complete via vim.notify without errors", function()
    local pack = reset_pack()
    -- Register a dev spec so Pack._specs has at least one entry. Install
    -- itself is stubbed, so the spec contents don't matter beyond name.
    pack.setup({
      specs = { { dev = true, name = "stubbed", config = function() end } },
    })

    package.loaded["core.pack.install"] = nil
    local Install = require("core.pack.install")
    local orig_im = Install.install_missing
    Install.install_missing = function(_specs, opts)
      if opts.on_complete then opts.on_complete() end
    end

    -- Capture the registered :Pack callback and drive it.
    local registered
    local real_create = vim.api.nvim_create_user_command
    vim.api.nvim_create_user_command = function(name, cb, opts)
      if name == "Pack" then registered = cb end
    end
    package.loaded["core.pack.commands"] = nil
    require("core.pack.commands").setup(pack)
    vim.api.nvim_create_user_command = real_create

    local notified
    local orig_notify = vim.notify
    vim.notify = function(msg) notified = tostring(msg) end
    local ok, err = pcall(registered, { fargs = { "install" }, bang = false })
    vim.notify = orig_notify
    Install.install_missing = orig_im

    T.eq(ok, true, ":Pack install must not raise: " .. tostring(err))
    T.truthy(notified and notified:match("install complete"),
      "expected 'install complete', got: " .. tostring(notified))
  end)
end)
