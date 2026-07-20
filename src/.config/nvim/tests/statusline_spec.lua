local T = require("tests.helpers")

local function reset_sl()
  package.loaded["lib.statusline"] = nil
  return require("lib.statusline")
end

T.describe("lib.statusline segment builders", function()
  T.it("mode_segment returns mode glyph + name", function()
    local sl = reset_sl()
    local s = sl._segments.mode("n")
    T.truthy(s:match("NORMAL"))
    s = sl._segments.mode("i")
    T.truthy(s:match("INSERT"))
  end)

  T.it("scrollbar returns a compact 2-char glyph", function()
    local sl = reset_sl()
    local bar = sl._segments.scrollbar(50, 100)
    T.eq(vim.fn.strchars(bar), 2)
  end)

  T.it("short_path truncates long paths", function()
    local sl = reset_sl()
    local short = sl._segments._short_path("/a/b/c/d/e/f/g/h.lua", 10)
    T.truthy(#short <= 25)
    T.truthy(short:match("…") ~= nil)
  end)
end)
