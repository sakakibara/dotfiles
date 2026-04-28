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
    T.truthy(lines[2]:match("%[x%]") and lines[2]:match("a") and lines[2]:match("abc1234..def8901") and lines[2]:match("3 commits") and lines[2]:match("feat: x"))
    T.truthy(lines[3]:match("%[x%]") and lines[3]:match("b") and lines[3]:match("1 commit") and lines[3]:match("fix: y"))
    view:close()
  end)

  T.it("toggle_at flips marked state and updates header counter", function()
    local UI = fresh()
    local pending = { { name = "a", from = "1", to = "2", count = 1, subject = "x" }, { name = "b", from = "3", to = "4", count = 2, subject = "y" } }
    local view = UI.update_review(pending, { open_window = false, on_apply = function() end })
    view:toggle_at(2)
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("1 of 2 marked"))
    T.truthy(lines[2]:match("%[ %]"))
    T.truthy(lines[3]:match("%[x%]"))
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

  T.it("opts.keymaps overrides default toggle", function()
    local UI = fresh()
    -- Module-level default
    T.eq(UI.keymaps.update_review.toggle, "<Tab>")
    -- Per-call override constructs without error
    local pending = { { name = "a", from = "1", to = "2", count = 1, subject = "x" } }
    local view = UI.update_review(pending, {
      open_window = false,
      on_apply = function() end,
      keymaps = { toggle = "<Space>" },
    })
    T.truthy(view.buf)
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

  T.it("done marks row as completed (v)", function()
    local UI = fresh()
    local view = UI.fidget({ open_window = false })
    view:set_status("plugin-a", "fetching")
    view:done("plugin-a")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("v"))
    view:close()
  end)

  T.it("error marks row as failed (x)", function()
    local UI = fresh()
    local view = UI.fidget({ open_window = false })
    view:set_status("plugin-a", "fetching")
    view:error("plugin-a", "timeout")
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("x") and lines[1]:match("timeout"))
    view:close()
  end)
end)

T.describe("core.pack.ui.clean_review", function()
  T.it("renders one row per orphan, default marked", function()
    local UI = fresh()
    local items = {
      { name = "old-a", dir = "/tmp/old-a", size_kb = 1024 },
      { name = "old-b", dir = "/tmp/old-b", size_kb = 512 },
    }
    local view = UI.clean_review(items, { open_window = false, on_apply = function() end })
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("2 of 2 marked") and lines[1]:match("MB"))
    T.truthy(lines[2]:match("%[x%]") and lines[2]:match("old%-a") and lines[2]:match("MB"))
    T.truthy(lines[3]:match("%[x%]") and lines[3]:match("old%-b"))
    view:close()
  end)

  T.it("toggle_at flips a row's mark", function()
    local UI = fresh()
    local items = { { name = "a", dir = "/tmp/a" } }
    local view = UI.clean_review(items, { open_window = false, on_apply = function() end })
    view:toggle_at(2)
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[2]:match("%[ %]"))
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

T.describe("core.pack.ui.rollback_review", function()
  T.it("renders one row per snapshot with iso date and plugin count", function()
    local UI = fresh()
    local snapshots = {
      { ts = 1714293600, iso = "2026-04-28T12:00:00Z", path = "/tmp/a.json", plugin_count = 88 },
      { ts = 1714207200, iso = "2026-04-27T12:00:00Z", path = "/tmp/b.json", plugin_count = 87 },
    }
    local view = UI.rollback_review(snapshots, { open_window = false, on_select = function() end })
    local lines = vim.api.nvim_buf_get_lines(view.buf, 0, -1, false)
    T.truthy(lines[1]:match("2 snapshots"))
    T.truthy(lines[3]:match("2026%-04%-28") and lines[3]:match("88"))
    T.truthy(lines[4]:match("2026%-04%-27") and lines[4]:match("87"))
    view:close()
  end)

  T.it("select_at calls on_select with the chosen snapshot", function()
    local UI = fresh()
    local snapshots = {
      { ts = 1714293600, iso = "2026-04-28T12:00:00Z", path = "/tmp/a.json", plugin_count = 88 },
      { ts = 1714207200, iso = "2026-04-27T12:00:00Z", path = "/tmp/b.json", plugin_count = 87 },
    }
    local got
    local view = UI.rollback_review(snapshots, { open_window = false, on_select = function(s) got = s end })
    view:select_at(4)  -- second snapshot row (header + blank + first + second)
    T.eq(got and got.ts, 1714207200)
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

T.describe("core.pack.ui.plan_columns", function()
  T.it("includes all columns when width fits", function()
    local UI = fresh()
    local plan = UI.plan_columns(
      { a = 10, b = 10, c = 10 },
      { a = 100, b = 80, c = 60 },
      100
    )
    T.eq(plan.a, true); T.eq(plan.b, true); T.eq(plan.c, true)
  end)

  T.it("drops lowest-priority column when too narrow", function()
    local UI = fresh()
    local plan = UI.plan_columns(
      { a = 30, b = 30, c = 30 },
      { a = 100, b = 80, c = 60 },
      80
    )
    T.eq(plan.c, false)  -- lowest priority dropped first
    T.truthy(plan.a and plan.b)
  end)

  T.it("drops multiple columns until width fits", function()
    local UI = fresh()
    local plan = UI.plan_columns(
      { a = 50, b = 50, c = 50 },
      { a = 100, b = 80, c = 60 },
      60
    )
    T.eq(plan.b, false); T.eq(plan.c, false)
    T.eq(plan.a, true)
  end)
end)
