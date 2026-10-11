local T = require("tests.helpers")

local EXPR = "%!v:lua.Lib.statuscolumn.render()"

local function column(win, lnum)
  return vim.api.nvim_eval_statusline(EXPR, { winid = win, use_statuscol_lnum = lnum }).str
end

local function two_windows(lines)
  vim.cmd("tabnew")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  local other = vim.api.nvim_get_current_win()
  vim.cmd("vsplit")
  local current = vim.api.nvim_get_current_win()
  for _, w in ipairs({ other, current }) do
    vim.wo[w].foldmethod = "manual"
    vim.wo[w].foldlevel = 0
  end
  return current, other
end

local function cleanup()
  vim.cmd("tabclose!")
end

T.describe("lib.statuscolumn", function()
  T.it("draws a non-current window's own number options", function()
    local current, other = two_windows({ "a", "b", "c", "d" })
    vim.wo[current].number, vim.wo[current].relativenumber = true, false
    vim.wo[other].number, vim.wo[other].relativenumber = false, false
    vim.api.nvim_win_set_cursor(current, { 1, 0 })
    T.truthy(column(current, 3):match("^%s*3%s*$"), "current window shows its line number")
    T.eq(column(other, 3):match("%d"), nil, "other window has numbering off")
    cleanup()
  end)

  T.it("draws a non-current window's relative numbers from its own cursor", function()
    local current, other = two_windows({ "a", "b", "c", "d", "e", "f" })
    for _, w in ipairs({ current, other }) do
      vim.wo[w].number, vim.wo[w].relativenumber = false, true
    end
    vim.api.nvim_win_set_cursor(current, { 1, 0 })
    vim.api.nvim_win_set_cursor(other, { 6, 0 })
    T.eq(vim.trim(column(other, 2)), "4")
    T.eq(vim.trim(column(current, 2)), "1")
    cleanup()
  end)

  T.it("draws a non-current window's own folds", function()
    local current, other = two_windows({ "a", "b", "c", "d", "e" })
    vim.api.nvim_win_call(other, function() vim.cmd("2,4fold") end)
    local glyph = vim.opt.fillchars:get().foldclose or ">"
    T.truthy(column(other, 2):find(glyph, 1, true), "other window shows its closed fold")
    T.eq(column(current, 2):find(glyph, 1, true), nil, "current window has no fold")
    cleanup()
  end)

  T.it("draws each window's fold glyph from its own window-local fillchars", function()
    local current, other = two_windows({ "a", "b", "c", "d", "e" })
    vim.wo[current].fillchars = "foldclose:C"
    vim.wo[other].fillchars = "foldclose:O"
    for _, w in ipairs({ current, other }) do
      vim.api.nvim_win_call(w, function() vim.cmd("2,4fold") end)
    end
    T.truthy(column(current, 2):find("C", 1, true), "current window uses its own glyph")
    T.truthy(column(other, 2):find("O", 1, true), "other window uses its own glyph")
    vim.wo[other].fillchars = "foldclose:Z"
    T.truthy(column(other, 2):find("Z", 1, true), "a changed window-local value is picked up")
    cleanup()
  end)

  T.it("relative numbers follow a fold closed or opened without a buffer change", function()
    local current = two_windows({ "a", "b", "c", "d", "e", "f" })
    vim.wo[current].number, vim.wo[current].relativenumber = false, true
    vim.api.nvim_win_set_cursor(current, { 6, 0 })
    T.eq(vim.trim(column(current, 1)), "5")
    local tick = vim.api.nvim_buf_get_changedtick(0)
    vim.cmd("2,4fold")
    T.eq(vim.api.nvim_buf_get_changedtick(0), tick)
    T.eq(vim.trim(column(current, 1)), "3")
    vim.cmd("normal! zR")
    T.eq(vim.trim(column(current, 1)), "5")
    cleanup()
  end)
end)
