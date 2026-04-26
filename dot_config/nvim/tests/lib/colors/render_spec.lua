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

T.describe("lib.colors.render diff invariant", function()
  local C = require("lib.colors.color")

  T.it("re-applying same detected list reuses extmarks (no churn)", function()
    R.reset()
    local buf = make_buf({ "x: #ff0000; y: #00ff00;" })
    local detected = {
      { lnum = 0, col_s = 3,  col_e = 10, color = C.from_hex("#ff0000") },
      { lnum = 0, col_s = 15, col_e = 22, color = C.from_hex("#00ff00") },
    }
    R.apply(buf, detected)
    local marks_a = get_marks(buf)
    R.apply(buf, detected)
    local marks_b = get_marks(buf)
    T.eq(marks_a[1][1], marks_b[1][1], "mark id changed for unchanged color")
    T.eq(marks_a[2][1], marks_b[2][1], "mark id changed for unchanged color")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("removing a color from list deletes its mark only", function()
    R.reset()
    local buf = make_buf({ "x: #ff0000; y: #00ff00;" })
    R.apply(buf, {
      { lnum = 0, col_s = 3,  col_e = 10, color = C.from_hex("#ff0000") },
      { lnum = 0, col_s = 15, col_e = 22, color = C.from_hex("#00ff00") },
    })
    R.apply(buf, {
      { lnum = 0, col_s = 3,  col_e = 10, color = C.from_hex("#ff0000") },
    })
    local marks = get_marks(buf)
    T.eq(#marks, 1, "expected 1 mark after removal, got " .. #marks)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

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

T.describe("lib.colors.render bigfile regression", function()
  T.it("4MB buffer with 1.8MB single line produces 0 extmarks", function()
    R.reset()
    local huge = string.rep("a", 1800000) .. " #ff0000"  -- 1.8MB line
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { huge })

    local D = require("lib.colors.detect")
    local detected = D.detect(buf, 0, 0)
    T.eq(#detected, 0, "expected detect to skip oversized line, got " .. #detected)

    R.apply(buf, detected)
    local marks = vim.api.nvim_buf_get_extmarks(buf, R.ns, 0, -1, {})
    T.eq(#marks, 0)

    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)
