local T = require("tests.helpers")
local F = require("lib.colors.format")
local C = require("lib.colors.color")

T.describe("lib.colors.format", function()
  T.it("hex (default) emits 6-digit hex", function()
    T.eq(F.format(C.from_hex("#ff0000"), "hex"), "#ff0000")
    T.eq(F.format(C.from_hex("#abcdef"), "hex"), "#abcdef")
  end)

  T.it("rgb default to space-separated", function()
    T.eq(F.format(C.from_hex("#ff0000"), "rgb"), "rgb(255 0 0)")
  end)

  T.it("rgb honors source.commas for legacy comma syntax", function()
    T.eq(F.format(C.from_hex("#ff0000"), "rgb", { commas = true }), "rgb(255, 0, 0)")
  end)

  T.it("rgba with_alpha emits 4-component", function()
    local color = C.from_hex("#ff0000"); color.a = 0.5
    T.eq(F.format(color, "rgb", { with_alpha = true, commas = true }),
         "rgba(255, 0, 0, 0.50)")
  end)

  T.it("rgba modern uses slash-alpha syntax (fn defaults to rgba w/ alpha)", function()
    local color = C.from_hex("#ff0000"); color.a = 0.5
    T.eq(F.format(color, "rgb", { with_alpha = true }),
         "rgba(255 0 0 / 0.50)")
  end)

  T.it("modern slash-alpha keeps fn=rgb when explicitly set", function()
    local color = C.from_hex("#ff0000"); color.a = 0.5
    T.eq(F.format(color, "rgb", { with_alpha = true, fn_name = "rgb" }),
         "rgb(255 0 0 / 0.50)")
  end)

  T.it("rgba alpha=1 emits literal '1' (not 1.00)", function()
    local color = C.from_hex("#ff0000"); color.a = 1
    T.eq(F.format(color, "rgb", { with_alpha = true, commas = true }),
         "rgba(255, 0, 0, 1)")
  end)

  T.it("hsl emits %s and degree h with no unit", function()
    -- pure red ≈ hsl(0 100% 50%)
    local out = F.format(C.from_hex("#ff0000"), "hsl")
    T.eq(out, "hsl(0 100% 50%)")
  end)

  T.it("oklch emits 3-decimal L,C and 1-decimal h", function()
    local out = F.format(C.from_hex("#ff0000"), "oklch")
    T.truthy(out:match("^oklch%(%d%.%d+ %d%.%d+ %d+%.%d%)$"))
  end)

  T.it("oklab emits 3 decimals each component", function()
    local out = F.format(C.from_hex("#ff0000"), "oklab")
    T.truthy(out:match("^oklab%(%-?%d%.%d+ %-?%d%.%d+ %-?%d%.%d+%)$"))
  end)

  T.it("formats() returns the supported set sorted", function()
    T.eq(F.formats(), { "hex", "hsl", "oklab", "oklch", "rgb" })
  end)

  T.it("is_format gates valid names", function()
    T.eq(F.is_format("hex"),   true)
    T.eq(F.is_format("oklch"), true)
    T.eq(F.is_format("bogus"), false)
  end)

  T.it("preserves rgba/rgb fn_name override (e.g. when source said rgba)", function()
    local color = C.from_hex("#ff0000"); color.a = 0.5
    T.eq(F.format(color, "rgb", { fn_name = "rgba", with_alpha = true, commas = true }),
         "rgba(255, 0, 0, 0.50)")
  end)
end)
