local T = require("tests.helpers")
local function fresh() package.loaded["core.pack.ui"] = nil; return require("core.pack.ui") end

T.describe("core.pack.ui.progress", function()
  T.it("opens a buffer with one line per item, all 'pending'", function()
    local UI = fresh()
    local view = UI.progress({ "a", "b", "c" }, { open_window = false })
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    -- Expect a header line + 3 status lines.
    T.truthy(#lines >= 4)
    T.truthy(lines[2]:match("^%s*%[ %] a "))
    T.truthy(lines[3]:match("^%s*%[ %] b "))
    T.truthy(lines[4]:match("^%s*%[ %] c "))
    view:close()
  end)

  T.it("set_status updates the matching line in place", function()
    local UI = fresh()
    local view = UI.progress({ "a", "b" }, { open_window = false })
    view:set_status("a", "ok", "cloned in 1.2s")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[2]:match("^%s*%[✓%] a .*cloned"))
    view:close()
  end)

  T.it("set_status with 'error' marks the line", function()
    local UI = fresh()
    local view = UI.progress({ "a" }, { open_window = false })
    view:set_status("a", "error", "git failed")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[2]:match("^%s*%[✗%] a "))
    view:close()
  end)
end)

T.describe("core.pack.ui.update_review", function()
  T.it("renders one line per pending update, default marked", function()
    local UI = fresh()
    local pending = {
      { name = "a", from = "abc1234567", to = "def8901234", count = 3 },
      { name = "b", from = "111", to = "222", count = 1 },
    }
    local view = UI.update_review(pending, { open_window = false, on_apply = function() end })
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[2]:match("^%s*%[x%] a "))
    T.truthy(lines[3]:match("^%s*%[x%] b "))
    view:close()
  end)

  T.it("toggle_at flips a line's mark", function()
    local UI = fresh()
    local pending = { { name = "a", from = "1", to = "2", count = 1 } }
    local view = UI.update_review(pending, { open_window = false, on_apply = function() end })
    view:toggle_at(2)
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[2]:match("^%s*%[ %] a "))
    view:close()
  end)

  T.it("apply() invokes on_apply with currently-marked entries only", function()
    local UI = fresh()
    local pending = {
      { name = "a", from = "1", to = "2", count = 1 },
      { name = "b", from = "1", to = "2", count = 1 },
    }
    local got
    local view = UI.update_review(pending, { open_window = false, on_apply = function(list) got = list end })
    view:toggle_at(3)  -- unmark b
    view:apply()
    T.eq(#got, 1)
    T.eq(got[1].name, "a")
    view:close()
  end)
end)
