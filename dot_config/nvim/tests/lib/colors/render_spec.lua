local T = require("tests.helpers")
local R = require("lib.colors.render")

local function make_buf(lines)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  return buf
end

local function get_marks(buf)
  return vim.api.nvim_buf_get_extmarks(buf, R.ns, 0, -1, { details = true })
end

T.describe("lib.colors.render virtual swatch", function()
  T.it("places one extmark per detected color", function()
    R.reset()
    local buf = make_buf({ "color: #ff0000;" })
    R.apply(buf, {
      { lnum = 0, col_s = 7, col_e = 14, color = require("lib.colors.color").from_hex("#ff0000") },
    })
    local marks = get_marks(buf)
    T.eq(#marks, 1)
    T.eq(marks[1][2], 0)   -- lnum
    T.eq(marks[1][3], 14)  -- col after literal
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("creates a highlight group LibColorsFg_<hex>", function()
    R.reset()
    local buf = make_buf({ "x: #00ff00;" })
    R.apply(buf, {
      { lnum = 0, col_s = 3, col_e = 10, color = require("lib.colors.color").from_hex("#00ff00") },
    })
    local hl = vim.api.nvim_get_hl(0, { name = "LibColorsFg_00ff00" })
    T.truthy(hl.fg ~= nil, "expected LibColorsFg_00ff00 hl group")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)
