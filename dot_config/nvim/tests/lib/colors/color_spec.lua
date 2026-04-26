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

T.describe("lib.colors.color hsl conversion", function()
  T.it("from_hsl converts to rgb (red)", function()
    local c = C.from_hsl(0, 1, 0.5)
    T.truthy(math.abs(c.r - 1) < 1e-6)
    T.truthy(math.abs(c.g - 0) < 1e-6)
    T.truthy(math.abs(c.b - 0) < 1e-6)
  end)

  T.it("from_hsl converts to rgb (cyan)", function()
    local c = C.from_hsl(180, 1, 0.5)
    T.truthy(math.abs(c.r - 0) < 1e-6)
    T.truthy(math.abs(c.g - 1) < 1e-6)
    T.truthy(math.abs(c.b - 1) < 1e-6)
  end)

  T.it("to_hsl round-trips from rgb", function()
    local rgb = { r = 1, g = 136/255, b = 0, a = 1 }
    local h, s, l = C.to_hsl(rgb)
    T.truthy(math.abs(h - 32) < 1, "hue ~32, got " .. h)
    T.truthy(math.abs(s - 1) < 0.01, "sat ~1, got " .. s)
    T.truthy(math.abs(l - 0.5) < 0.01, "light ~0.5, got " .. l)
  end)
end)

T.describe("lib.colors.color oklch conversion", function()
  T.it("from_oklch produces sRGB in 0..1 range", function()
    local c = C.from_oklch(0.5, 0.1, 30)
    T.truthy(c.r >= 0 and c.r <= 1, "r out of range: " .. c.r)
    T.truthy(c.g >= 0 and c.g <= 1, "g out of range: " .. c.g)
    T.truthy(c.b >= 0 and c.b <= 1, "b out of range: " .. c.b)
  end)

  T.it("white round-trips", function()
    local white = { r = 1, g = 1, b = 1, a = 1 }
    local l, c, h = C.to_oklch(white)
    T.truthy(math.abs(l - 1) < 0.01, "L ~1, got " .. l)
    T.truthy(c < 0.01, "C ~0 for white, got " .. c)
  end)

  T.it("black round-trips", function()
    local black = { r = 0, g = 0, b = 0, a = 1 }
    local l = C.to_oklch(black)
    T.truthy(l < 0.01, "L ~0 for black, got " .. l)
  end)

  T.it("oklch(0.74 0.16 50) approximates #ff8800", function()
    local c = C.from_oklch(0.74, 0.16, 50)
    T.truthy(math.abs(c.r - 1.0) < 0.05, "r near 1.0, got " .. c.r)
    T.truthy(math.abs(c.g - 0.533) < 0.1, "g near 0.533, got " .. c.g)
    T.truthy(math.abs(c.b - 0.0) < 0.1, "b near 0, got " .. c.b)
  end)
end)
