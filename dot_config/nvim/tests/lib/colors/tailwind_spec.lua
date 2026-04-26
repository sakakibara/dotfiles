local T = require("tests.helpers")
local TW = require("lib.colors.tailwind")

T.describe("lib.colors.tailwind palette lookup", function()
  T.it("resolves red-500 to a Color", function()
    local c = TW.resolve("red-500")
    T.truthy(c, "resolve returned nil")
    T.truthy(c.source.fmt == "oklch")
    -- Tailwind v4 red-500 ≈ oklch(0.637 0.237 25.331); should be red-ish
    T.truthy(c.r > 0.7 and c.r < 1.0, "expected red-ish r, got " .. c.r)
  end)

  T.it("resolves blue-500", function()
    local c = TW.resolve("blue-500")
    T.truthy(c)
    T.truthy(c.b > 0.6, "expected blue-ish b, got " .. c.b)
  end)

  T.it("returns nil for unknown classes", function()
    T.eq(TW.resolve("not-a-class-500"), nil)
  end)

  T.it("strips utility prefix (bg-/text-/border-/...)", function()
    local c = TW.resolve_class("bg-red-500")
    T.truthy(c)
    local c2 = TW.resolve_class("text-blue-500")
    T.truthy(c2)
  end)
end)

T.describe("lib.colors.tailwind project @theme scan", function()
  T.it("scan_file harvests --color-* declarations from @theme", function()
    TW._overlay = {}
    local fixture = vim.fn.getcwd() .. "/tests/lib/colors/fixtures/theme.css"
    TW.scan_file(fixture)
    T.truthy(TW._overlay["brand-500"], "brand-500 not in overlay")
    T.truthy(TW._overlay["warn"], "warn not in overlay")
  end)

  T.it("resolve uses overlay before built-in", function()
    TW._overlay = { ["red-500"] = { 0.5, 0.5, 0 } }
    local c = TW.resolve("red-500")
    T.truthy(c, "resolve returned nil")
    -- Reset for downstream tests
    TW._overlay = {}
  end)
end)
