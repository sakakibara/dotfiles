local T = require("tests.helpers")
local D = require("lib.colors.detect")

local function make_buf(lines)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  return buf
end

T.describe("lib.colors.detect regex fallback", function()
  T.it("finds hex literals on multiple lines", function()
    local buf = make_buf({ "color: #ff0000;", "bg: #00ff00;" })
    local results = D.detect(buf, 0, 1)
    T.eq(#results, 2)
    T.eq(results[1].lnum, 0)
    T.eq(results[2].lnum, 1)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("respects 1000-char per-line cap", function()
    local big = "a:" .. string.rep("x", 1500) .. " #ff0000;"
    local buf = make_buf({ big })
    local results = D.detect(buf, 0, 0)
    T.eq(#results, 0, "should skip lines > 1000 chars")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("respects 256KB total visible cap", function()
    -- 300 lines × 1000 chars = 300KB > 256KB
    local lines = {}
    for i = 1, 300 do lines[i] = string.rep("x", 999) .. " #ff0000" end
    local buf = make_buf(lines)
    local results = D.detect(buf, 0, 299)
    T.eq(#results, 0, "should skip when viewport bytes > 256KB")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

T.describe("lib.colors.detect TS dispatch", function()
  T.it("uses TS for CSS buffers", function()
    local buf = make_buf({
      ".btn {",
      "  color: #ff0000;",
      "  /* #00ff00 — should be skipped (inside a comment) */",
      "}",
    })
    vim.bo[buf].filetype = "css"
    local results = D.detect(buf, 0, 3)
    -- TS query should exclude the comment
    T.eq(#results, 1, "expected 1 (excluding comment), got " .. #results)
    T.eq(results[1].lnum, 1)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)
