local T = require("tests.helpers")
local A = require("lib.colors.augends")

T.describe("lib.colors.augends find()", function()
  local lightness = A.lightness()

  T.it("finds a hex literal under the cursor", function()
    -- "color: #ff0000;"
    --         ^ col=8 (1-indexed) is inside the literal
    local hit = lightness.find("color: #ff0000;", 9)
    T.truthy(hit, "expected a find hit")
    T.eq(hit.text, "#ff0000")
    T.eq(hit.from, 8)
    T.eq(hit.to,   14)
  end)

  T.it("returns nil when the cursor isn't on a color", function()
    T.eq(lightness.find("no colors here", 5), nil)
  end)

  T.it("handles rgb() literals", function()
    local hit = lightness.find("c: rgb(255, 0, 0);", 8)
    T.truthy(hit)
    T.eq(hit.text, "rgb(255, 0, 0)")
  end)
end)

T.describe("lib.colors.augends add()", function()
  T.it("lightness +1 step bumps L by ~0.05", function()
    local lightness = A.lightness({ step = 0.05 })
    -- pure red has L ≈ 0.628 in OKLCH
    local out = lightness.add("#ff0000", 1, 1)
    T.truthy(out.text:match("^#%x%x%x%x%x%x$"), "expected hex output, got " .. out.text)
    T.truthy(out.text ~= "#ff0000", "expected text to change")
  end)

  T.it("lightness -1 step lowers L by ~0.05", function()
    local lightness = A.lightness({ step = 0.05 })
    local up   = lightness.add("#7f7f7f", 1,  1).text
    local down = lightness.add("#7f7f7f", -1, 1).text
    -- Up should be lighter than original; down should be darker.
    local C = require("lib.colors.color")
    local Lo = ({ C.to_oklch(C.from_hex("#7f7f7f")) })[1]
    local Lu = ({ C.to_oklch(C.from_hex(up))     })[1]
    local Ld = ({ C.to_oklch(C.from_hex(down))   })[1]
    T.truthy(Lu > Lo,  "expected lighter L after +1, got " .. Lu .. " vs " .. Lo)
    T.truthy(Ld < Lo,  "expected darker  L after -1, got " .. Ld .. " vs " .. Lo)
  end)

  T.it("hue wraps around 360", function()
    local hue = A.hue({ step = 30 })
    local C = require("lib.colors.color")
    -- One full rotation (12 * 30° = 360°) should land back near the
    -- original h — pure red is ~29° in OKLCH, not 0°, so we compare
    -- against the source h rather than 0.
    local _, _, h0 = C.to_oklch(C.from_hex("#ff0000"))
    local out      = hue.add("#ff0000", 12, 1).text
    local _, _, h1 = C.to_oklch(C.from_hex(out))
    local d        = math.min(math.abs(h1 - h0), math.abs(h1 - h0 - 360), math.abs(h1 - h0 + 360))
    T.truthy(d < 5, "expected h≈" .. h0 .. " after wrap, got " .. h1)
  end)

  T.it("chroma cannot go below 0", function()
    local chroma = A.chroma({ step = 1 })  -- 1 is huge; force underflow
    local out = chroma.add("#888888", -10, 1).text
    local C = require("lib.colors.color")
    local _, Cval = C.to_oklch(C.from_hex(out))
    T.truthy(Cval >= 0, "expected chroma >= 0, got " .. Cval)
  end)

  T.it("preserves source format (rgb stays rgb)", function()
    local lightness = A.lightness()
    local out = lightness.add("rgb(255, 0, 0)", 1, 1).text
    T.truthy(out:match("^rgb%(%d+, %d+, %d+%)$"),
             "expected rgb(...) form, got " .. out)
  end)

  T.it("preserves source format (oklch stays oklch)", function()
    local lightness = A.lightness()
    local out = lightness.add("oklch(0.5 0.1 90)", 1, 1).text
    T.truthy(out:match("^oklch%(.+%)$"),
             "expected oklch(...) form, got " .. out)
  end)
end)
