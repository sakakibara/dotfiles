local T = require("tests.helpers")
local P = require("lib.colors.picker")
local C = require("lib.colors.color")

T.describe("lib.colors.picker state", function()
  T.it("open creates a state with current color", function()
    local s = P.open({ initial = C.from_hex("#ff0000") })
    T.truthy(s)
    T.eq(s.color.r, 1)
    T.eq(s.color.g, 0)
    T.eq(s.color.b, 0)
    T.eq(s.mode, "compact")
    T.eq(s.space, "rgb")
    P.close(s)
  end)

  T.it("close marks state as closed", function()
    local s = P.open({ initial = C.from_hex("#00ff00") })
    P.close(s)
    T.eq(s.mode, "closed")
  end)

  T.it("toggle_expand cycles compact ↔ expanded", function()
    local s = P.open({ initial = C.from_hex("#0000ff") })
    P.toggle_expand(s)
    T.eq(s.mode, "expanded")
    P.toggle_expand(s)
    T.eq(s.mode, "compact")
    P.close(s)
  end)
end)

T.describe("lib.colors.picker compact render", function()
  T.it("open creates a floating buffer with current color displayed", function()
    local s = P.open({ initial = C.from_hex("#ff0000") })
    T.truthy(s.buf and vim.api.nvim_buf_is_valid(s.buf), "expected valid floating buf")
    local lines = vim.api.nvim_buf_get_lines(s.buf, 0, -1, false)
    T.truthy(#lines >= 4, "expected ≥4 lines, got " .. #lines)
    local found = false
    for _, l in ipairs(lines) do
      if l:find("#ff0000", 1, true) then found = true; break end
    end
    T.truthy(found, "expected #ff0000 in some line, got: " .. table.concat(lines, "\\n"))
    P.close(s)
  end)
end)

T.describe("lib.colors.picker swatch rendering", function()
  T.it("creates an extmark with bg-colored hl group on the swatch line", function()
    local s = P.open({ initial = C.from_hex("#ff0000") })
    local marks = vim.api.nvim_buf_get_extmarks(s.buf, P.ns, 0, -1, { details = true })
    T.truthy(#marks >= 1, "expected at least 1 extmark on swatch line")
    local found = false
    for _, m in ipairs(marks) do
      local d = m[4]
      if d and d.hl_group and d.hl_group:find("ff0000") then found = true; break end
    end
    T.truthy(found, "expected an hl_group with ff0000")
    P.close(s)
  end)
end)
