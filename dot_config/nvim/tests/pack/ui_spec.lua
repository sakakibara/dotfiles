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
  T.it("renders tabular row per pending update with elpaca-style layout", function()
    local UI = fresh()
    local pending = {
      { name = "a", from = "abc1234567", to = "def8901234", count = 3, subject = "feat: x" },
      { name = "b", from = "111", to = "222", count = 1, subject = "fix: y" },
    }
    local view = UI.update_review(pending, { open_window = false, on_apply = function() end })
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    -- Header line summarizes counts + keymap legend.
    T.truthy(lines[1]:match("2 of 2 marked"))
    T.truthy(lines[1]:match("<Tab> toggle"))
    -- Per-row format: status glyph + name + from..to + count + subject.
    T.truthy(lines[2]:match("●") and lines[2]:match("a") and lines[2]:match("abc1234..def8901") and lines[2]:match("3 commits") and lines[2]:match("feat: x"))
    T.truthy(lines[3]:match("●") and lines[3]:match("b") and lines[3]:match("1 commit") and lines[3]:match("fix: y"))
    view:close()
  end)

  T.it("toggle_at flips marked state and updates header counter", function()
    local UI = fresh()
    local pending = { { name = "a", from = "1", to = "2", count = 1, subject = "x" }, { name = "b", from = "3", to = "4", count = 2, subject = "y" } }
    local view = UI.update_review(pending, { open_window = false, on_apply = function() end })
    view:toggle_at(2)
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("1 of 2 marked"))
    T.truthy(lines[2]:match("◯"))
    T.truthy(lines[3]:match("●"))
    view:close()
  end)

  T.it("apply() invokes on_apply with currently-marked entries only", function()
    local UI = fresh()
    local pending = { { name = "a", from = "1", to = "2", count = 1, subject = "x" }, { name = "b", from = "3", to = "4", count = 1, subject = "y" } }
    local got
    local view = UI.update_review(pending, { open_window = false, on_apply = function(list) got = list end })
    view:toggle_at(3)  -- unmark b
    view:apply()
    T.eq(#got, 1)
    T.eq(got[1].name, "a")
    view:close()
  end)

  T.it("set_all marks or unmarks all rows", function()
    local UI = fresh()
    local pending = { { name = "a", from = "1", to = "2", count = 1, subject = "x" }, { name = "b", from = "3", to = "4", count = 1, subject = "y" } }
    local view = UI.update_review(pending, { open_window = false, on_apply = function() end })
    view:set_all(false)
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("0 of 2 marked"))
    view:set_all(true)
    lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("2 of 2 marked"))
    view:close()
  end)
end)

T.describe("core.pack.ui.status", function()
  T.it("renders a buffer containing the supplied lines", function()
    local UI = fresh()
    local view = UI.status({ "Packs: 3 registered", "  a  loaded", "  b  lazy" }, { open_window = false })
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.eq(#lines, 3)
    T.eq(lines[1], "Packs: 3 registered")
    T.truthy(lines[2]:match("^%s+a%s+loaded"))
    view:close()
  end)

  T.it("close() tears down buffer", function()
    local UI = fresh()
    local view = UI.status({ "x" }, { open_window = false })
    local buf = view.buf
    view:close()
    T.eq(vim.api.nvim_buf_is_valid(buf), false)
  end)
end)
