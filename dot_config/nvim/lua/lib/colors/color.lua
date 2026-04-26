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

-- sRGB transfer (gamma) functions
local function srgb_to_linear(c)
  if c <= 0.04045 then return c / 12.92 end
  return ((c + 0.055) / 1.055) ^ 2.4
end

local function linear_to_srgb(c)
  if c <= 0.0031308 then return 12.92 * c end
  return 1.055 * (c ^ (1/2.4)) - 0.055
end

-- Linear sRGB <-> OKLab (Björn Ottosson's matrices)
local function linear_srgb_to_oklab(r, g, b)
  local l = 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b
  local m = 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b
  local s = 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b
  l, m, s = l ^ (1/3), m ^ (1/3), s ^ (1/3)
  return
    0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s,
    1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
    0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s
end

local function oklab_to_linear_srgb(L, a, b)
  local l = L + 0.3963377774 * a + 0.2158037573 * b
  local m = L - 0.1055613458 * a - 0.0638541728 * b
  local s = L - 0.0894841775 * a - 1.2914855480 * b
  l, m, s = l ^ 3, m ^ 3, s ^ 3
  return
    4.0767416621  * l - 3.3077115913  * m + 0.2309699292 * s,
   -1.2684380046  * l + 2.6097574011  * m - 0.3413193965 * s,
   -0.0041960863  * l - 0.7034186147  * m + 1.7076147010 * s
end

function M.from_oklch(L, C, h, alpha)
  local rad = math.rad(h)
  local a = C * math.cos(rad)
  local b = C * math.sin(rad)
  local lr, lg, lb = oklab_to_linear_srgb(L, a, b)
  return {
    r = clamp(linear_to_srgb(lr), 0, 1),
    g = clamp(linear_to_srgb(lg), 0, 1),
    b = clamp(linear_to_srgb(lb), 0, 1),
    a = alpha or 1,
    space = "srgb",
    source = { fmt = "oklch" },
  }
end

function M.to_oklch(c)
  local lr = srgb_to_linear(c.r)
  local lg = srgb_to_linear(c.g)
  local lb = srgb_to_linear(c.b)
  local L, a, b = linear_srgb_to_oklab(lr, lg, lb)
  local C = math.sqrt(a*a + b*b)
  local h = math.deg(math.atan2(b, a))
  if h < 0 then h = h + 360 end
  return L, C, h
end

return M
