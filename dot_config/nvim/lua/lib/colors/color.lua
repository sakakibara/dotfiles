-- lua/lib/colors/color.lua
-- Pure color math. No nvim API calls.
local M = {}

---@class Color
---@field r number  -- 0..1
---@field g number  -- 0..1
---@field b number  -- 0..1
---@field a number  -- 0..1
---@field space? "srgb"|"p3"
---@field source? table  -- {fmt = "hex"|"rgb"|"hsl"|"oklch"|"named"|...}

local function clamp(x, lo, hi) return math.max(lo, math.min(hi, x)) end

-- hex parsing: supports #rgb, #rrggbb, #rrggbbaa (case-insensitive)
function M.from_hex(s)
  if type(s) ~= "string" then return nil end
  local hex = s:match("^#(%x+)$")
  if not hex then return nil end
  local len = #hex
  local r, g, b, a
  if len == 3 then
    r = tonumber(hex:sub(1, 1):rep(2), 16)
    g = tonumber(hex:sub(2, 2):rep(2), 16)
    b = tonumber(hex:sub(3, 3):rep(2), 16)
    a = 255
  elseif len == 6 then
    r = tonumber(hex:sub(1, 2), 16)
    g = tonumber(hex:sub(3, 4), 16)
    b = tonumber(hex:sub(5, 6), 16)
    a = 255
  elseif len == 8 then
    r = tonumber(hex:sub(1, 2), 16)
    g = tonumber(hex:sub(3, 4), 16)
    b = tonumber(hex:sub(5, 6), 16)
    a = tonumber(hex:sub(7, 8), 16)
  else
    return nil
  end
  return { r = r/255, g = g/255, b = b/255, a = a/255, space = "srgb", source = { fmt = "hex" } }
end

function M.to_hex(c)
  local r = clamp(math.floor(c.r * 255 + 0.5), 0, 255)
  local g = clamp(math.floor(c.g * 255 + 0.5), 0, 255)
  local b = clamp(math.floor(c.b * 255 + 0.5), 0, 255)
  local a = clamp(math.floor((c.a or 1) * 255 + 0.5), 0, 255)
  if a == 255 then
    return string.format("#%02x%02x%02x", r, g, b)
  end
  return string.format("#%02x%02x%02x%02x", r, g, b, a)
end

return M
