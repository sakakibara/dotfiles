-- lua/lib/colors/harmony.lua
-- Pure color-harmony helpers operating in OKLCH for perceptual hue rotation.
local C = require("lib.colors.color")
local M = {}

local function rotate(color, deg)
  local L, Cval, h = C.to_oklch(color)
  local hn = (h + deg) % 360
  return C.from_oklch(L, Cval, hn, color.a)
end

function M.complementary(color)
  return rotate(color, 180)
end

function M.triad(color)
  return rotate(color, 120), rotate(color, 240)
end

function M.analogous(color)
  return rotate(color, -30), rotate(color, 30)
end

return M
