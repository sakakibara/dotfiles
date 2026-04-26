local T = require("tests.helpers")
local C = require("lib.colors.color")

T.describe("lib.colors.color hex round-trip", function()
  T.it("from_hex parses #rrggbb", function()
    local c = C.from_hex("#ff8800")
    T.truthy(c, "from_hex returned nil")
    T.eq(c.r, 255/255)
    T.eq(c.g, 136/255)
    T.eq(c.b, 0)
    T.eq(c.a, 1)
  end)

  T.it("from_hex parses #rgb shorthand", function()
    local c = C.from_hex("#f80")
    T.eq(c.r, 255/255)
    T.eq(c.g, 136/255)
    T.eq(c.b, 0)
  end)

  T.it("from_hex parses #rrggbbaa", function()
    local c = C.from_hex("#ff880080")
    T.eq(c.a, 128/255)
  end)

  T.it("to_hex produces #rrggbb", function()
    local c = { r = 1, g = 136/255, b = 0, a = 1 }
    T.eq(C.to_hex(c), "#ff8800")
  end)

  T.it("to_hex with alpha < 1 produces #rrggbbaa", function()
    local c = { r = 1, g = 136/255, b = 0, a = 128/255 }
    T.eq(C.to_hex(c), "#ff880080")
  end)

  T.it("from_hex returns nil on garbage", function()
    T.eq(C.from_hex("not a color"), nil)
    T.eq(C.from_hex("#zzzzzz"), nil)
    T.eq(C.from_hex("#1234"), nil)
  end)
end)
