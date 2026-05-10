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

-- CIE Lab / Lch math (D50 reference white, per CSS Color Module Level 4)
-- Reference: https://www.w3.org/TR/css-color-4/#color-conversion-code

-- D50 white point (CSS uses D50 for lab/lch)
local D50 = { 0.96422, 1.0, 0.82521 }

-- Bradford chromatic adaptation matrix: D50 → D65
local D50_to_D65 = {
  {  0.9554734527042182, -0.0230985368742614,  0.0632593086610217 },
  { -0.0283697069632081,  1.0099954580058226,  0.0210413531719399 },
  {  0.0123140016883199, -0.0205076964334779,  1.3303659366080753 },
}

-- D65 linear sRGB matrix from CIE XYZ
local XYZ_D65_to_linear_sRGB = {
  {  3.2409699419045226, -1.5373831775700935, -0.4986107602930034 },
  { -0.9692436362808796,  1.8759675015077202,  0.0415550574071756 },
  {  0.0556300796969936, -0.2039769588889765,  1.0569715142428786 },
}

local function mat3_mul(M, v)
  return
    M[1][1]*v[1] + M[1][2]*v[2] + M[1][3]*v[3],
    M[2][1]*v[1] + M[2][2]*v[2] + M[2][3]*v[3],
    M[3][1]*v[1] + M[3][2]*v[2] + M[3][3]*v[3]
end

-- Lab (D50) → CIE XYZ (D50)
local function lab_to_xyz_d50(L, a, b)
  local fy = (L + 16) / 116
  local fx = a / 500 + fy
  local fz = fy - b / 200
  local function finv(t)
    local t3 = t * t * t
    if t3 > 216/24389 then return t3 end
    return (116 * t - 16) / 903.3
  end
  return finv(fx) * D50[1], finv(fy) * D50[2], finv(fz) * D50[3]
end

function M.from_lab(L, a, b, alpha)
  local x, y, z = lab_to_xyz_d50(L, a, b)
  local x65, y65, z65 = mat3_mul(D50_to_D65, { x, y, z })
  local lr, lg, lb = mat3_mul(XYZ_D65_to_linear_sRGB, { x65, y65, z65 })
  return {
    r = clamp(linear_to_srgb(lr), 0, 1),
    g = clamp(linear_to_srgb(lg), 0, 1),
    b = clamp(linear_to_srgb(lb), 0, 1),
    a = alpha or 1,
    space = "srgb",
    source = { fmt = "lab" },
  }
end

function M.from_lch(L, C, h, alpha)
  local rad = math.rad(h)
  local a = C * math.cos(rad)
  local b = C * math.sin(rad)
  local c = M.from_lab(L, a, b, alpha)
  c.source = { fmt = "lch" }
  return c
end

-- Display-P3 → CIE XYZ (D65). Reference: CSS Color Module Level 4.
local P3_to_XYZ_D65 = {
  { 0.4865709486482162,   0.26566769316909306, 0.1982172852343625 },
  { 0.2289745640697488,   0.6917385218365064,  0.079286914093745  },
  { 0.0000000000000000,   0.04511338185890264, 1.043944368900976  },
}

function M.from_p3(r, g, b, alpha)
  -- Display-P3 uses the sRGB transfer function (same gamma curve).
  local lr = srgb_to_linear(r)
  local lg = srgb_to_linear(g)
  local lb = srgb_to_linear(b)
  -- P3 linear → CIE XYZ (D65) → linear sRGB
  local x, y, z = mat3_mul(P3_to_XYZ_D65, { lr, lg, lb })
  local r2, g2, b2 = mat3_mul(XYZ_D65_to_linear_sRGB, { x, y, z })
  return {
    r = clamp(linear_to_srgb(r2), 0, 1),
    g = clamp(linear_to_srgb(g2), 0, 1),
    b = clamp(linear_to_srgb(b2), 0, 1),
    a = alpha or 1,
    space = "p3",
    source = { fmt = "p3" },
  }
end

function M.from_oklab(L, a, b, alpha)
  local lr, lg, lb = oklab_to_linear_srgb(L, a, b)
  return {
    r = clamp(linear_to_srgb(lr), 0, 1),
    g = clamp(linear_to_srgb(lg), 0, 1),
    b = clamp(linear_to_srgb(lb), 0, 1),
    a = alpha or 1,
    space = "srgb",
    source = { fmt = "oklab" },
  }
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

-- WCAG relative luminance (linear sRGB weighted by CIE Y coefficients).
-- Exposed publicly so consumers (contrast.lua, picker swatch readability,
-- ad-hoc scripts) don't have to re-derive it.
function M.relative_luminance(c)
  local lr = srgb_to_linear(c.r)
  local lg = srgb_to_linear(c.g)
  local lb = srgb_to_linear(c.b)
  return 0.2126 * lr + 0.7152 * lg + 0.0722 * lb
end

-- WCAG contrast ratio between two colors (1:1 = same luminance, up to 21:1
-- for white-on-black). Order-independent; the result is always >= 1.
function M.contrast_ratio(c1, c2)
  local L1 = M.relative_luminance(c1)
  local L2 = M.relative_luminance(c2)
  if L1 < L2 then L1, L2 = L2, L1 end
  return (L1 + 0.05) / (L2 + 0.05)
end

-- Returns the highest WCAG level the ratio satisfies for normal text:
-- "AAA" (>= 7), "AA" (>= 4.5), or "fail" (< 4.5). Large-text thresholds
-- (3 / 4.5) are not a separate level here — callers can compute as needed.
function M.contrast_level(ratio)
  if ratio >= 7   then return "AAA"  end
  if ratio >= 4.5 then return "AA"   end
  return "fail"
end

-- Pick black or white text for a given background such that contrast is maximized.
function M.contrast_text(bg)
  local L = M.relative_luminance(bg)
  local on_black = (L + 0.05) / 0.05
  local on_white = 1.05 / (L + 0.05)
  return on_white > on_black and "#ffffff" or "#000000"
end

return M
