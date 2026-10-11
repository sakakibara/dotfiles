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

  T.it("mode_segment labels full mode(1) strings", function()
    local sl = reset_sl()
    T.truthy(sl._segments.mode("niI"):match("NORMAL"))
    T.truthy(sl._segments.mode("nov"):match("O%-PEND"))
    T.truthy(sl._segments.mode("Rvc"):match("V%-RPLCE"))
    T.truthy(sl._segments.mode("ntT"):match("NORMAL"))
    T.truthy(sl._segments.mode("cvr"):match("EX"))
  end)
end)
