local T = require("tests.helpers")
local function fresh() package.loaded["core.pack.ui"] = nil; return require("core.pack.ui") end

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

  T.it("toggle_expand calls on_expand for missing _log", function()
    local UI = fresh()
    local pending = { { name = "a", from = "1", to = "2", count = 1, subject = "x" } }
    local expand_called_for
    local view = UI.update_review(pending, {
      open_window = false,
      on_apply = function() end,
      on_expand = function(name, cb)
        expand_called_for = name
        cb({ { sha = "abcdef0", subject = "test commit" } })
      end,
    })
    view:toggle_expand(2)
    T.eq(expand_called_for, "a")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[3]:match("abcdef0") and lines[3]:match("test commit"), "expanded log line not rendered")
    view:close()
  end)
end)

T.describe("core.pack.ui.fidget", function()
  T.it("set_status adds a row with name and text", function()
    local UI = fresh()
    local view = UI.fidget({ open_window = false })
    view:set_status("plugin-a", "fetching")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("plugin%-a") and lines[1]:match("fetching"))
    view:close()
  end)

  T.it("set_status updates an existing row in place", function()
    local UI = fresh()
    local view = UI.fidget({ open_window = false })
    view:set_status("plugin-a", "fetching")
    view:set_status("plugin-a", "resolving")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.eq(#lines, 1)
    T.truthy(lines[1]:match("resolving"))
    view:close()
  end)

  T.it("done marks row as completed (✓)", function()
    local UI = fresh()
    local view = UI.fidget({ open_window = false })
    view:set_status("plugin-a", "fetching")
    view:done("plugin-a")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("✓"))
    view:close()
  end)

  T.it("error marks row as failed (✗)", function()
    local UI = fresh()
    local view = UI.fidget({ open_window = false })
    view:set_status("plugin-a", "fetching")
    view:error("plugin-a", "timeout")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("✗") and lines[1]:match("timeout"))
    view:close()
  end)
end)

T.describe("core.pack.ui.clean_review", function()
  T.it("renders one row per orphan, default marked", function()
    local UI = fresh()
    local items = { { name = "old-a", dir = "/tmp/old-a" }, { name = "old-b", dir = "/tmp/old-b" } }
    local view = UI.clean_review(items, { open_window = false, on_apply = function() end })
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("2 of 2 marked"))
    T.truthy(lines[2]:match("●") and lines[2]:match("old%-a"))
    T.truthy(lines[3]:match("●") and lines[3]:match("old%-b"))
    view:close()
  end)

  T.it("toggle_at flips a row's mark", function()
    local UI = fresh()
    local items = { { name = "a", dir = "/tmp/a" } }
    local view = UI.clean_review(items, { open_window = false, on_apply = function() end })
    view:toggle_at(2)
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[2]:match("◯"))
    view:close()
  end)

  T.it("apply invokes on_apply with marked entries only", function()
    local UI = fresh()
    local items = { { name = "a", dir = "/tmp/a" }, { name = "b", dir = "/tmp/b" } }
    local got
    local view = UI.clean_review(items, { open_window = false, on_apply = function(list) got = list end })
    view:toggle_at(3)  -- unmark b
    view:apply()
    T.eq(#got, 1)
    T.eq(got[1].name, "a")
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

  T.it("status applies opts.highlights via extmarks", function()
    local UI = fresh()
    local view = UI.status({ "header", "  body" }, {
      open_window = false,
      highlights = {
        { 0, 0, 6, "Title" },
        { 1, 2, 6, "Identifier" },
      },
    })
    local marks = vim.api.nvim_buf_get_extmarks(view.buf, -1, 0, -1, { details = true })
    T.truthy(#marks >= 2)
    view:close()
  end)
end)
