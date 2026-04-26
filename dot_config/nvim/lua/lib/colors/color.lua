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

-- HSL: h in degrees 0..360, s and l in 0..1
function M.from_hsl(h, s, l, a)
  h = (h % 360) / 360
  local function hue2rgb(p, q, t)
    if t < 0 then t = t + 1 end
    if t > 1 then t = t - 1 end
    if t < 1/6 then return p + (q - p) * 6 * t end
    if t < 1/2 then return q end
    if t < 2/3 then return p + (q - p) * (2/3 - t) * 6 end
    return p
  end
  local r, g, b
  if s == 0 then
    r, g, b = l, l, l
  else
    local q = l < 0.5 and l * (1 + s) or l + s - l * s
    local p = 2 * l - q
    r = hue2rgb(p, q, h + 1/3)
    g = hue2rgb(p, q, h)
    b = hue2rgb(p, q, h - 1/3)
  end
  return { r = r, g = g, b = b, a = a or 1, space = "srgb", source = { fmt = "hsl" } }
end

function M.to_hsl(c)
  local r, g, b = c.r, c.g, c.b
  local maxc = math.max(r, g, b)
  local minc = math.min(r, g, b)
  local h, s
  local l = (maxc + minc) / 2
  if maxc == minc then
    h, s = 0, 0
  else
    local d = maxc - minc
    s = l > 0.5 and d / (2 - maxc - minc) or d / (maxc + minc)
    if maxc == r then
      h = (g - b) / d + (g < b and 6 or 0)
    elseif maxc == g then
      h = (b - r) / d + 2
    else
      h = (r - g) / d + 4
    end
    h = h * 60
  end
  return h, s, l
end

return M
