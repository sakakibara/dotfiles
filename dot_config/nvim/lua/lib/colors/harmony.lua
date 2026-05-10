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

local TW_PALETTE = require("lib.colors._tailwind_palette")
local NAMED      = require("lib.colors._named")

-- Perceptual ΔE in OKLab: √((L1−L2)² + (a1−a2)² + (b1−b2)²)
local function oklab_dist(c1, c2)
  local L1, Cval1, h1 = C.to_oklch(c1)
  local a1, b1 = Cval1 * math.cos(math.rad(h1)), Cval1 * math.sin(math.rad(h1))
  local L2, Cval2, h2 = C.to_oklch(c2)
  local a2, b2 = Cval2 * math.cos(math.rad(h2)), Cval2 * math.sin(math.rad(h2))
  local dL, da, db = L1 - L2, a1 - a2, b1 - b2
  return math.sqrt(dL*dL + da*da + db*db)
end

function M.nearest_tailwind(color)
  local best_name, best_dist
  for name, triple in pairs(TW_PALETTE) do
    if name ~= "white" and name ~= "black" then
      local cand = C.from_oklch(triple[1], triple[2], triple[3])
      local d = oklab_dist(color, cand)
      if not best_dist or d < best_dist then
        best_name, best_dist = name, d
      end
    end
  end
  return best_name, best_dist
end

function M.nearest_named(color)
  local best_name, best_dist
  for name, hex in pairs(NAMED) do
    if name ~= "transparent" then
      local cand = C.from_hex(hex)
      if cand then
        local d = oklab_dist(color, cand)
        if not best_dist or d < best_dist then
          best_name, best_dist = name, d
        end
      end
    end
  end
  return best_name, best_dist
end

return M
