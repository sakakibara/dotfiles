-- Color → text serialization in any of the supported color formats.
-- Extracted from picker.lua's commit path so :ColorConvert / :ColorYank
-- can reuse the same logic, and so adding a new format only touches one
-- place.
--
-- API:
--   format.format(color, fmt, source_opts) -> string
--     color  — { r, g, b, a, ... } as produced by parse / from_*
--     fmt    — "hex" | "rgb" | "hsl" | "oklch" | "oklab"
--     source_opts — optional table; preserves the original literal's
--                   surface syntax when round-tripping rgb/hsl:
--       fn_name    — "rgb" / "rgba" / "hsl" / "hsla" (defaults from with_alpha)
--       commas     — true to use "r, g, b" instead of "r g b"
--       with_alpha — true to emit the alpha channel (rgba / hsla shape)
--
-- Modern slash-alpha shape (`rgb(r g b / a)`) is used when commas=false
-- and with_alpha=true, matching the picker's prior behavior.

local C = require("lib.colors.color")
local M = {}

local FORMATS = { hex = true, rgb = true, hsl = true, oklch = true, oklab = true }

function M.formats()
  local out = {}
  for k in pairs(FORMATS) do out[#out + 1] = k end
  table.sort(out)
  return out
end

function M.is_format(fmt) return FORMATS[fmt] == true end

local function fmt_alpha(a)
  if a == 1 then return "1" end
  return string.format("%.2f", a)
end

local function format_rgb(color, src)
  local r = math.floor(color.r * 255 + 0.5)
  local g = math.floor(color.g * 255 + 0.5)
  local b = math.floor(color.b * 255 + 0.5)
  local fn  = src.fn_name or (src.with_alpha and "rgba" or "rgb")
  local sep = src.commas and ", " or " "
  if src.with_alpha then
    local a_str = fmt_alpha(color.a or 1)
    if src.commas then
      return string.format("%s(%d, %d, %d, %s)", fn, r, g, b, a_str)
    end
    return string.format("%s(%d %d %d / %s)", fn, r, g, b, a_str)
  end
  return string.format("%s(%d%s%d%s%d)", fn, r, sep, g, sep, b)
end

local function format_hsl(color, src)
  local h, s, l = C.to_hsl(color)
  local hi, si, li = math.floor(h), math.floor(s * 100), math.floor(l * 100)
  local fn  = src.fn_name or (src.with_alpha and "hsla" or "hsl")
  local sep = src.commas and ", " or " "
  if src.with_alpha then
    local a_str = fmt_alpha(color.a or 1)
    if src.commas then
      return string.format("%s(%d, %d%%, %d%%, %s)", fn, hi, si, li, a_str)
    end
    return string.format("%s(%d %d%% %d%% / %s)", fn, hi, sep, si, sep, li)
  end
  return string.format("%s(%d%s%d%%%s%d%%)", fn, hi, sep, si, sep, li)
end

local function format_oklch(color)
  local L, Cval, h = C.to_oklch(color)
  return string.format("oklch(%.3f %.3f %.1f)", L, Cval, h)
end

local function format_oklab(color)
  local L, Cval, h = C.to_oklch(color)
  local a = Cval * math.cos(math.rad(h))
  local b = Cval * math.sin(math.rad(h))
  return string.format("oklab(%.3f %.3f %.3f)", L, a, b)
end

function M.format(color, fmt, source_opts)
  source_opts = source_opts or {}
  if fmt == "rgb"   then return format_rgb(color, source_opts)   end
  if fmt == "hsl"   then return format_hsl(color, source_opts)   end
  if fmt == "oklch" then return format_oklch(color)              end
  if fmt == "oklab" then return format_oklab(color)              end
  return C.to_hex(color)   -- hex (default)
end

return M
