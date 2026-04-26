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

T.describe("lib.colors.picker adjustment", function()
  T.it("adjust(+1) on R increases r by 1/255", function()
    local s = P.open({ initial = C.from_hex("#000000") })
    P.adjust(s, 1)
    local r = math.floor(s.color.r * 255 + 0.5)
    T.eq(r, 1)
    P.close(s)
  end)

  T.it("cycle_slider moves to next component and wraps", function()
    local s = P.open({ initial = C.from_hex("#ff0000") })
    T.eq(s.slider, 1)
    P.cycle_slider(s); T.eq(s.slider, 2)
    P.cycle_slider(s); T.eq(s.slider, 3)
    P.cycle_slider(s); T.eq(s.slider, 1)
    P.close(s)
  end)

  T.it("cycle_space cycles rgb → hsl → oklch → rgb and resets slider", function()
    local s = P.open({ initial = C.from_hex("#ff0000") })
    T.eq(s.space, "rgb")
    P.cycle_slider(s)  -- slider = 2
    P.cycle_space(s)
    T.eq(s.space, "hsl")
    T.eq(s.slider, 1)  -- reset
    P.cycle_space(s); T.eq(s.space, "oklch")
    P.cycle_space(s); T.eq(s.space, "rgb")
    P.close(s)
  end)
end)

T.describe("lib.colors.picker keymaps", function()
  T.it("l increases active component by 1", function()
    local s = P.open({ initial = C.from_hex("#7f0000") })
    vim.api.nvim_buf_call(s.buf, function() vim.cmd("normal l") end)
    local r = math.floor(s.color.r * 255 + 0.5)
    T.eq(r, 0x80)
    P.close(s)
  end)

  T.it("Tab cycles space", function()
    local s = P.open({ initial = C.from_hex("#ff0000") })
    vim.api.nvim_buf_call(s.buf, function()
      local tab = vim.api.nvim_replace_termcodes("<Tab>", true, false, true)
      vim.api.nvim_feedkeys(tab, "x", false)
    end)
    T.eq(s.space, "hsl")
    P.close(s)
  end)

  T.it("Esc closes the picker", function()
    local s = P.open({ initial = C.from_hex("#ff0000") })
    vim.api.nvim_buf_call(s.buf, function() vim.cmd("normal \27") end)
    T.eq(s.mode, "closed")
  end)
end)

T.describe("lib.colors.picker commit", function()
  T.it("CR replaces anchor range with new value in source format", function()
    local src = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_lines(src, 0, -1, false, { "color: #ff0000;" })
    -- Parse the hex to get a color whose source.fmt = "hex"
    local hit = require("lib.colors.parse").parse("color: #ff0000;", 9)
    T.truthy(hit, "expected parse to find hex literal")
    local s = P.open({
      initial = hit.color,
      anchor  = { buf = src, lnum = 0, col_s = hit.range.col_s, col_e = hit.range.col_e },
    })
    P.cycle_space(s)  -- → hsl
    P.adjust(s, 60)   -- hue +60
    P.commit(s)
    local line = vim.api.nvim_buf_get_lines(src, 0, 1, false)[1]
    T.truthy(line:match("color: #[0-9a-f]+;"), "expected hex in source, got: " .. line)
    T.truthy(not line:match("#ff0000"), "expected color to have changed")
    vim.api.nvim_buf_delete(src, { force = true })
  end)
end)
