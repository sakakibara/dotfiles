local T = require("tests.helpers")

local function reset_mc()
  package.loaded["lib.mode_color"] = nil
  return require("lib.mode_color")
end

local function is_hex(s)
  return type(s) == "string" and s:match("^#%x%x%x%x%x%x$") ~= nil
end

T.describe("lib.mode_color", function()
  T.it("target returns a valid #rrggbb hex for each mode", function()
    local mc = reset_mc()
    for _, mode in ipairs({ "n", "i", "v", "V", "c", "s", "R", "t" }) do
      T.truthy(is_hex(mc.target(mode)), "mode " .. mode)
    end
  end)

  T.it("unknown mode falls back to normal color", function()
    local mc = reset_mc()
    T.eq(mc.target("?"), mc.target("n"))
  end)

  T.it("interpolate produces midpoint correctly", function()
    local mc = reset_mc()
    T.eq(mc._interpolate("#000000", "#ffffff", 0.5), "#808080")
  end)

  T.it("different modes produce different colors", function()
    local mc = reset_mc()
    T.truthy(mc.target("n") ~= mc.target("i"), "normal vs insert should differ")
    T.truthy(mc.target("i") ~= mc.target("v"), "insert vs visual should differ")
  end)
end)
