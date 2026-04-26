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

  T.it("round-trips #ff8800 through oklch", function()
    local orig = { r = 1, g = 0x88/255, b = 0, a = 1 }
    local L, ch, h = C.to_oklch(orig)
    local back = C.from_oklch(L, ch, h)
    T.truthy(math.abs(back.r - orig.r) < 0.01, "r drift: " .. back.r)
    T.truthy(math.abs(back.g - orig.g) < 0.01, "g drift: " .. back.g)
    T.truthy(math.abs(back.b - orig.b) < 0.01, "b drift: " .. back.b)
  end)
end)

T.describe("lib.colors.color oklab conversion", function()
  T.it("from_oklab(0, 0, 0) is black", function()
    local c = C.from_oklab(0, 0, 0)
    T.truthy(math.abs(c.r) < 0.01 and math.abs(c.g) < 0.01 and math.abs(c.b) < 0.01,
      string.format("expected black, got (%.3f, %.3f, %.3f)", c.r, c.g, c.b))
  end)

  T.it("from_oklab(1, 0, 0) is white", function()
    local c = C.from_oklab(1, 0, 0)
    T.truthy(math.abs(c.r - 1) < 0.01 and math.abs(c.g - 1) < 0.01 and math.abs(c.b - 1) < 0.01)
  end)

  T.it("from_oklab and from_oklch agree on a chromatic color", function()
    -- For OKLCH (L, C, h), the equivalent OKLab is (L, C*cos(h), C*sin(h)).
    local L, Chroma, h = 0.74, 0.16, 50
    local rad = math.rad(h)
    local a, b = Chroma * math.cos(rad), Chroma * math.sin(rad)
    local c1 = C.from_oklab(L, a, b)
    local c2 = C.from_oklch(L, Chroma, h)
    T.truthy(math.abs(c1.r - c2.r) < 1e-6)
    T.truthy(math.abs(c1.g - c2.g) < 1e-6)
    T.truthy(math.abs(c1.b - c2.b) < 1e-6)
  end)
end)

T.describe("lib.colors.color CIELAB conversion", function()
  T.it("from_lab(0, 0, 0) is black", function()
    local c = C.from_lab(0, 0, 0)
    T.truthy(c.r < 0.01 and c.g < 0.01 and c.b < 0.01,
      string.format("expected black, got (%.3f, %.3f, %.3f)", c.r, c.g, c.b))
  end)

  T.it("from_lab(100, 0, 0) is approximately white", function()
    local c = C.from_lab(100, 0, 0)
    T.truthy(math.abs(c.r - 1) < 0.02 and math.abs(c.g - 1) < 0.02 and math.abs(c.b - 1) < 0.02,
      string.format("expected white, got (%.3f, %.3f, %.3f)", c.r, c.g, c.b))
  end)

  T.it("from_lab produces non-grey for non-zero a/b", function()
    local c = C.from_lab(50, 60, 0)  -- pinkish-red direction
    T.truthy(c.r > c.g, "expected r > g for positive a")
  end)
end)

T.describe("lib.colors.color CIELCH conversion", function()
  T.it("from_lch(0, 0, 0) is black", function()
    local c = C.from_lch(0, 0, 0)
    T.truthy(c.r < 0.01 and c.g < 0.01 and c.b < 0.01)
  end)

  T.it("from_lch(50, 60, 30) round-trips through lab", function()
    local rad = math.rad(30)
    local a, b = 60 * math.cos(rad), 60 * math.sin(rad)
    local c1 = C.from_lab(50, a, b)
    local c2 = C.from_lch(50, 60, 30)
    T.truthy(math.abs(c1.r - c2.r) < 1e-6)
    T.truthy(math.abs(c1.g - c2.g) < 1e-6)
    T.truthy(math.abs(c1.b - c2.b) < 1e-6)
  end)
end)

T.describe("lib.colors.color contrast_text", function()
  T.it("returns black for light colors", function()
    T.eq(C.contrast_text({ r = 1, g = 1, b = 1, a = 1 }), "#000000")
    T.eq(C.contrast_text({ r = 1, g = 1, b = 0, a = 1 }), "#000000")
  end)

  T.it("returns white for dark colors", function()
    T.eq(C.contrast_text({ r = 0, g = 0, b = 0, a = 1 }), "#ffffff")
    T.eq(C.contrast_text({ r = 0, g = 0, b = 0.4, a = 1 }), "#ffffff")
  end)
end)
