local T = require("tests.helpers")

T.describe("lib.colors", function()
  T.it("registers under Lib.colors", function()
    package.loaded["lib.colors"] = nil
    require("lib").init()
    T.truthy(Lib.colors, "Lib.colors not registered")
    T.truthy(type(Lib.colors.setup) == "function", "Lib.colors.setup missing")
  end)
end)

T.describe("lib.colors.setup", function()
  local C = require("lib.colors.color")

  T.it("setup() then BufReadPost triggers detection on a CSS buf", function()
    package.loaded["lib.colors.render"] = nil
    package.loaded["lib.colors"] = nil
    require("lib").init()
    Lib.colors.setup({})

    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "color: #ff0000;" })
    vim.bo[buf].filetype = "css"
    vim.api.nvim_set_current_buf(buf)
    vim.api.nvim_exec_autocmds("BufReadPost", { buffer = buf })

    -- Debouncer is 16ms; wait synchronously for it to fire.
    vim.wait(80, function()
      local R = require("lib.colors.render")
      return R._state[buf] and next(R._state[buf].by_key) ~= nil
    end)

    local R = require("lib.colors.render")
    local marks = vim.api.nvim_buf_get_extmarks(buf, R.ns, 0, -1, {})
    T.truthy(#marks >= 1, "expected at least 1 mark, got " .. #marks)

    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("setup() respects exclude_ft", function()
    package.loaded["lib.colors.render"] = nil
    package.loaded["lib.colors"] = nil
    require("lib").init()
    Lib.colors.setup({ exclude_ft = { "bigfile" } })

    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "color: #ff0000;" })
    vim.bo[buf].filetype = "bigfile"
    vim.api.nvim_set_current_buf(buf)
    vim.api.nvim_exec_autocmds("BufReadPost", { buffer = buf })

    vim.wait(80)

    local R = require("lib.colors.render")
    local marks = vim.api.nvim_buf_get_extmarks(buf, R.ns, 0, -1, {})
    T.eq(#marks, 0, "expected 0 marks for excluded ft, got " .. #marks)

    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

T.describe("lib.colors :ColorsToggle command", function()
  T.it("registers :ColorsToggle after setup()", function()
    package.loaded["lib.colors"] = nil
    require("lib").init()
    Lib.colors.setup({})
    local cmds = vim.api.nvim_get_commands({})
    T.truthy(cmds.ColorsToggle, ":ColorsToggle not registered")
  end)
end)

T.describe("lib.colors.schedule timer reuse", function()
  T.it("reuses the same uv_timer across rapid schedule() calls", function()
    package.loaded["lib.colors"] = nil
    _G.Lib = nil
    require("lib").init()
    Lib.colors.setup({})

    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_set_current_buf(buf)
    -- Trigger schedule many times in a row
    for _ = 1, 50 do vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf }) end
    -- All 50 should have ended up reusing the SAME timer handle
    local timer_count = 0
    for _ in pairs(Lib.colors._timers) do timer_count = timer_count + 1 end
    T.eq(timer_count, 1, "expected 1 timer entry, got " .. timer_count)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

T.describe("lib.colors toggle correctness", function()
  T.it("M.toggle(0) flips state for the current buffer", function()
    package.loaded["lib.colors"] = nil
    require("lib").init()
    Lib.colors.setup({})

    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_set_current_buf(buf)
    -- Pre-condition: not yet toggled, _enabled[buf] is nil (treated as enabled)
    Lib.colors.toggle(0)
    -- After toggle, _enabled keyed by the actual buffer handle should be false
    T.eq(Lib.colors._enabled[buf], false, "toggle(0) didn't flip _enabled[" .. buf .. "]")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it(":ColorsToggle global disables rendering for tracked buffers", function()
    package.loaded["lib.colors.render"] = nil
    package.loaded["lib.colors"] = nil
    require("lib").init()
    Lib.colors.setup({})

    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "color: #ff0000;" })
    vim.bo[buf].filetype = "css"
    vim.api.nvim_set_current_buf(buf)
    vim.api.nvim_exec_autocmds("BufReadPost", { buffer = buf })
    vim.wait(80, function()
      local R = require("lib.colors.render")
      return R._state[buf] and next(R._state[buf].by_key) ~= nil
    end)

    -- Sanity check: a mark exists
    local R = require("lib.colors.render")
    local marks_before = vim.api.nvim_buf_get_extmarks(buf, R.ns, 0, -1, {})
    T.truthy(#marks_before >= 1, "expected mark before toggle, got " .. #marks_before)

    -- Run :ColorsToggle global
    vim.cmd("ColorsToggle global")
    vim.wait(80)

    local marks_after = vim.api.nvim_buf_get_extmarks(buf, R.ns, 0, -1, {})
    T.eq(#marks_after, 0, "expected 0 marks after global toggle, got " .. #marks_after)

    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)
