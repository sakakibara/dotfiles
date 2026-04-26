-- lua/lib/colors/parse.lua
-- Pure: string + offset → { range = {col_s, col_e}, color = Color } or nil.
-- Each format has its own pattern. parse() returns the literal that contains
-- the given offset (or nil); parse_all() scans an entire string.
local C = require("lib.colors.color")
local NAMED = require("lib.colors._named")
local M = {}

-- Each detector: pattern (Lua), capture-to-Color converter
local detectors = {}

local function add_detector(pattern, to_color)
  table.insert(detectors, { pattern = pattern, to_color = to_color })
end

-- Hex: #rgb, #rrggbb, #rrggbbaa. The %f[%W%z] frontier requires the next
-- character to be non-word or end-of-string, so e.g. `#define` (where `def`
-- is hex but is part of the longer C identifier) is correctly skipped.
add_detector("#%x+%f[%W%z]", function(match)
  local n = #match - 1
  if n ~= 3 and n ~= 6 and n ~= 8 then return nil end
  return C.from_hex(match)
end)

-- rgb()/rgba() — legacy comma and modern space syntax, with optional / alpha
add_detector("rgba?%([^)]+%)", function(match)
  local inner = match:gsub("^rgba?%(", ""):gsub("%)$", "")
  inner = inner:gsub(",", " "):gsub("/", " ")
  local nums = {}
  for tok in inner:gmatch("%S+") do
    local pct = tok:match("^(%-?%d+%.?%d*)%%$")
    if pct then
      table.insert(nums, tonumber(pct) / 100 * 255)
    else
      local n = tonumber(tok)
      if n then table.insert(nums, n) end
    end
  end
  if #nums < 3 or #nums > 4 then return nil end
  local a = nums[4]
  if a then
    if a > 1 then a = a / 255 end
  end
  return {
    r = nums[1] / 255,
    g = nums[2] / 255,
    b = nums[3] / 255,
    a = a or 1,
    space = "srgb",
    source = { fmt = "rgb" },
  }
end)

-- hsl()/hsla() — h in deg/rad/turn (or unitless = deg), s and l in %
add_detector("hsla?%([^)]+%)", function(match)
  local inner = match:gsub("^hsla?%(", ""):gsub("%)$", "")
  inner = inner:gsub(",", " "):gsub("/", " ")
  local toks = {}
  for tok in inner:gmatch("%S+") do table.insert(toks, tok) end
  if #toks < 3 or #toks > 4 then return nil end

  local h_str = toks[1]
  local h_num, unit = h_str:match("^(%-?[%d%.]+)(%a*)$")
  if not h_num then return nil end
  local h = tonumber(h_num)
  if unit == "rad" then h = math.deg(h)
  elseif unit == "turn" then h = h * 360
  end

  local s_pct = toks[2]:match("^(%-?[%d%.]+)%%$")
  local l_pct = toks[3]:match("^(%-?[%d%.]+)%%$")
  if not s_pct or not l_pct then return nil end
  local s = tonumber(s_pct) / 100
  local l = tonumber(l_pct) / 100

  local a
  if toks[4] then
    local pct = toks[4]:match("^(%-?[%d%.]+)%%$")
    a = pct and (tonumber(pct) / 100) or tonumber(toks[4])
  end

  local c = C.from_hsl(h, s, l, a)
  c.source = { fmt = "hsl" }
  return c
end)

-- oklch()
add_detector("oklch%([^)]+%)", function(match)
  local inner = match:gsub("^oklch%(", ""):gsub("%)$", "")
  inner = inner:gsub("/", " ")
  local toks = {}
  for tok in inner:gmatch("%S+") do table.insert(toks, tok) end
  if #toks < 3 or #toks > 4 then return nil end

  local L
  local l_pct = toks[1]:match("^([%d%.]+)%%$")
  if l_pct then L = tonumber(l_pct) / 100 else L = tonumber(toks[1]) end

  local Cval = tonumber(toks[2])
  local h_num, unit = toks[3]:match("^(%-?[%d%.]+)(%a*)$")
  if not h_num then return nil end
  local h = tonumber(h_num)
  if unit == "rad" then h = math.deg(h)
  elseif unit == "turn" then h = h * 360
  end

  local a
  if toks[4] then
    local pct = toks[4]:match("^([%d%.]+)%%$")
    a = pct and (tonumber(pct) / 100) or tonumber(toks[4])
  end

  if not L or not Cval or not h then return nil end
  return C.from_oklch(L, Cval, h, a)
end)

-- oklab() — Cartesian OKLab. L is 0..1 or %, a and b are signed numbers.
add_detector("oklab%([^)]+%)", function(match)
  local inner = match:gsub("^oklab%(", ""):gsub("%)$", "")
  inner = inner:gsub("/", " ")
  local toks = {}
  for tok in inner:gmatch("%S+") do table.insert(toks, tok) end
  if #toks < 3 or #toks > 4 then return nil end

  local L
  local l_pct = toks[1]:match("^([%d%.]+)%%$")
  if l_pct then L = tonumber(l_pct) / 100 else L = tonumber(toks[1]) end

  local a = tonumber(toks[2])
  local b = tonumber(toks[3])

  local alpha
  if toks[4] then
    local pct = toks[4]:match("^([%d%.]+)%%$")
    alpha = pct and (tonumber(pct) / 100) or tonumber(toks[4])
  end

  if not L or not a or not b then return nil end
  return C.from_oklab(L, a, b, alpha)
end)

-- lab() — CSS CIELAB. L is 0..100 or %, a and b are signed numbers.
add_detector("lab%([^)]+%)", function(match)
  local inner = match:gsub("^lab%(", ""):gsub("%)$", "")
  inner = inner:gsub("/", " ")
  local toks = {}
  for tok in inner:gmatch("%S+") do table.insert(toks, tok) end
  if #toks < 3 or #toks > 4 then return nil end

  local L
  local l_pct = toks[1]:match("^([%d%.]+)%%$")
  if l_pct then L = tonumber(l_pct) else L = tonumber(toks[1]) end

  local a = tonumber(toks[2])
  local b = tonumber(toks[3])

  local alpha
  if toks[4] then
    local pct = toks[4]:match("^([%d%.]+)%%$")
    alpha = pct and (tonumber(pct) / 100) or tonumber(toks[4])
  end

  if not L or not a or not b then return nil end
  return C.from_lab(L, a, b, alpha)
end)

-- lch() — CSS CIELCH. L is 0..100 or %, C is 0..150-ish, h is angle.
add_detector("lch%([^)]+%)", function(match)
  local inner = match:gsub("^lch%(", ""):gsub("%)$", "")
  inner = inner:gsub("/", " ")
  local toks = {}
  for tok in inner:gmatch("%S+") do table.insert(toks, tok) end
  if #toks < 3 or #toks > 4 then return nil end

  local L
  local l_pct = toks[1]:match("^([%d%.]+)%%$")
  if l_pct then L = tonumber(l_pct) else L = tonumber(toks[1]) end

  local Cval = tonumber(toks[2])

  local h_num, unit = toks[3]:match("^(%-?[%d%.]+)(%a*)$")
  if not h_num then return nil end
  local h = tonumber(h_num)
  if unit == "rad" then h = math.deg(h)
  elseif unit == "turn" then h = h * 360
  end

  local alpha
  if toks[4] then
    local pct = toks[4]:match("^([%d%.]+)%%$")
    alpha = pct and (tonumber(pct) / 100) or tonumber(toks[4])
  end

  if not L or not Cval or not h then return nil end
  return C.from_lch(L, Cval, h, alpha)
end)

-- color(<space> r g b [/ alpha]) — currently only display-p3 is supported.
add_detector("color%([^)]+%)", function(match)
  local inner = match:gsub("^color%(", ""):gsub("%)$", "")
  inner = inner:gsub("/", " ")
  local toks = {}
  for tok in inner:gmatch("%S+") do table.insert(toks, tok) end
  if #toks < 4 or #toks > 5 then return nil end

  local space = toks[1]
  if space ~= "display-p3" then return nil end

  local function num_or_pct(s)
    local pct = s:match("^(%-?[%d%.]+)%%$")
    if pct then return tonumber(pct) / 100 end
    return tonumber(s)
  end

  local r = num_or_pct(toks[2])
  local g = num_or_pct(toks[3])
  local b = num_or_pct(toks[4])

  local alpha
  if toks[5] then
    local pct = toks[5]:match("^([%d%.]+)%%$")
    alpha = pct and (tonumber(pct) / 100) or tonumber(toks[5])
  end

  if not r or not g or not b then return nil end
  return C.from_p3(r, g, b, alpha)
end)

-- color-mix(in SPACE, c1 [PCT], c2 [PCT]) — interpolate two colors in the
-- given color space. Supported: srgb, oklch, oklab. Other spaces → nil.
local function _shortest_hue_lerp(h1, h2, t)
  -- CSS default: shortest path on the hue circle.
  local diff = h2 - h1
  if diff > 180 then diff = diff - 360
  elseif diff < -180 then diff = diff + 360
  end
  return (h1 + t * diff) % 360
end

add_detector("color%-mix%([^)]+%)", function(match)
  local inner = match:gsub("^color%-mix%(", ""):gsub("%)$", "")

  -- Split on commas (top-level) — but the colors themselves may contain
  -- commas (rgb(a, b, c)). For simplicity we leverage the fact that within
  -- color-mix the colors are space-separated functional notations like
  -- oklch(...) or hex/named. Parens-aware comma split:
  local parts = {}
  local depth = 0
  local cur = ""
  for i = 1, #inner do
    local ch = inner:sub(i, i)
    if ch == "(" then depth = depth + 1; cur = cur .. ch
    elseif ch == ")" then depth = depth - 1; cur = cur .. ch
    elseif ch == "," and depth == 0 then
      table.insert(parts, cur)
      cur = ""
    else cur = cur .. ch
    end
  end
  if cur ~= "" then table.insert(parts, cur) end
  if #parts ~= 3 then return nil end

  -- Trim each part
  local function trim(s) return s:match("^%s*(.-)%s*$") end
  local in_clause = trim(parts[1])
  local c1_clause = trim(parts[2])
  local c2_clause = trim(parts[3])

  -- Parse "in <space>"
  local space = in_clause:match("^in%s+([%w%-]+)$")
  if not space then return nil end
  -- Unsupported space: consume the range so inner named colors don't leak through.
  if space ~= "srgb" and space ~= "oklch" and space ~= "oklab" then return M._BLOCKED end

  -- Each color clause is "<color>" or "<color> <pct>%"
  local function parse_clause(s)
    -- Try "<color> N%" first (greedy match on color + trailing percent)
    local color_str, pct = s:match("^(.+)%s+([%d%.]+)%%$")
    if color_str then
      local results = M.parse_all(color_str)
      if #results == 0 then return nil end
      return results[1].color, tonumber(pct)
    end
    -- No percent given
    local results = M.parse_all(s)
    if #results == 0 then return nil end
    return results[1].color, nil
  end

  local c1, p1 = parse_clause(c1_clause)
  local c2, p2 = parse_clause(c2_clause)
  if not c1 or not c2 then return nil end

  -- Normalize percentages: defaults are 50/50; if only one is given, the
  -- other = 100 - that. If both, normalize so they sum to 100.
  if not p1 and not p2 then p1, p2 = 50, 50
  elseif p1 and not p2 then p2 = 100 - p1
  elseif p2 and not p1 then p1 = 100 - p2
  else
    local sum = p1 + p2
    if sum > 0 then p1, p2 = p1 / sum * 100, p2 / sum * 100 end
  end
  local t = p2 / 100  -- fraction of c2

  local mixed
  if space == "srgb" then
    mixed = {
      r = c1.r * (1 - t) + c2.r * t,
      g = c1.g * (1 - t) + c2.g * t,
      b = c1.b * (1 - t) + c2.b * t,
      a = (c1.a or 1) * (1 - t) + (c2.a or 1) * t,
      space = "srgb",
      source = { fmt = "color-mix" },
    }
  elseif space == "oklab" then
    -- Convert each to OKLab via oklch (oklab is the cartesian form).
    local L1, Ch1, h1 = C.to_oklch(c1)
    local a1 = Ch1 * math.cos(math.rad(h1))
    local b1 = Ch1 * math.sin(math.rad(h1))
    local L2, Ch2, h2 = C.to_oklch(c2)
    local a2 = Ch2 * math.cos(math.rad(h2))
    local b2 = Ch2 * math.sin(math.rad(h2))
    local Lm = L1 * (1 - t) + L2 * t
    local am = a1 * (1 - t) + a2 * t
    local bm = b1 * (1 - t) + b2 * t
    mixed = C.from_oklab(Lm, am, bm, (c1.a or 1) * (1 - t) + (c2.a or 1) * t)
    mixed.source = { fmt = "color-mix" }
  else  -- oklch
    local L1, Ch1, h1 = C.to_oklch(c1)
    local L2, Ch2, h2 = C.to_oklch(c2)
    local Lm = L1 * (1 - t) + L2 * t
    local Cm = Ch1 * (1 - t) + Ch2 * t
    local hm = _shortest_hue_lerp(h1, h2, t)
    mixed = C.from_oklch(Lm, Cm, hm, (c1.a or 1) * (1 - t) + (c2.a or 1) * t)
    mixed.source = { fmt = "color-mix" }
  end

  return mixed
end)

-- CSS named colors. Use Lua frontier patterns (%f[%a] / %f[^%a]) to require
-- letter boundaries on both sides, so that "red" inside "border-radius" is
-- not matched.
add_detector("%f[%a][%a]+%f[^%a]", function(match)
  local hex = NAMED[match:lower()]
  if not hex then return nil end
  local c = C.from_hex(hex)
  c.source = { fmt = "named", original = match }
  return c
end)

-- Tailwind utility class. Detector matches conservatively (any "word-dash-stop"
-- token); resolve_class/resolve filter to actual palette entries, so non-color
-- classes like "border-radius-4" produce nil and are dropped.
local TW = require("lib.colors.tailwind")
add_detector("[%w%-]+%-%d+", function(match)
  return TW.resolve_class(match) or TW.resolve(match)
end)

-- Tailwind arbitrary values: bg-[#abc], bg-[oklch(...)], etc. The bracketed
-- inner value is a complete color literal in some other format (underscores
-- stand in for spaces in the source). Strip the prefix + brackets, then
-- re-parse the inner.
add_detector("[%w%-]+%-%[[^%]]+%]", function(match)
  local inner = match:match("%[([^%]]+)%]")
  if not inner then return nil end
  inner = inner:gsub("_", " ")
  local results = M.parse_all(inner)
  if #results == 0 then return nil end
  return results[1].color
end)

-- Sentinel returned by detectors that want to consume a range without
-- producing a visible color (e.g. color-mix with an unsupported space).
-- parse_all uses it for overlap suppression only; it is not included in output.
M._BLOCKED = {}

-- Scan str for all literals; return list of {range, color}.
function M.parse_all(str)
  local raw = {}
  for _, d in ipairs(detectors) do
    local pos = 1
    while pos <= #str do
      local s, e = str:find("(" .. d.pattern .. ")", pos)
      if not s then break end
      local match = str:sub(s, e)
      local color = d.to_color(match)
      if color then
        -- _BLOCKED entries participate in overlap suppression but are not emitted.
        table.insert(raw, { range = { col_s = s - 1, col_e = e }, color = color, _blocked = (color == M._BLOCKED) })
      end
      pos = e + 1
    end
  end
  -- Sort by start position; for ties prefer longer spans (larger col_e first).
  table.sort(raw, function(a, b)
    if a.range.col_s ~= b.range.col_s then return a.range.col_s < b.range.col_s end
    return a.range.col_e > b.range.col_e
  end)
  -- Remove overlapping entries: keep whichever was sorted first (longest span
  -- at each position). A result overlaps the accepted set if its start falls
  -- before the end of the last accepted entry.
  local results = {}
  local frontier = -1
  for _, r in ipairs(raw) do
    if r.range.col_s >= frontier then
      if not r._blocked then table.insert(results, r) end
      frontier = r.range.col_e
    end
  end
  return results
end

function M.parse(str, offset)
  for _, r in ipairs(M.parse_all(str)) do
    if offset >= r.range.col_s and offset < r.range.col_e then
      return r
    end
  end
  return nil
end

return M
