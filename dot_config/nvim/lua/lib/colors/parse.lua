-- lua/lib/colors/parse.lua
-- Pure: string + offset → { range = {col_s, col_e}, color = Color } or nil.
-- Each format has its own pattern. parse() returns the literal that contains
-- the given offset (or nil); parse_all() scans an entire string.
local C = require("lib.colors.color")
local M = {}

-- Each detector: pattern (Lua), capture-to-Color converter
local detectors = {}

local function add_detector(pattern, to_color)
  table.insert(detectors, { pattern = pattern, to_color = to_color })
end

-- Hex: #rgb, #rrggbb, #rrggbbaa
add_detector("#%x+", function(match)
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

-- Scan str for all literals; return list of {range, color}.
function M.parse_all(str)
  local results = {}
  for _, d in ipairs(detectors) do
    local pos = 1
    while pos <= #str do
      local s, e = str:find("(" .. d.pattern .. ")", pos)
      if not s then break end
      local match = str:sub(s, e)
      local color = d.to_color(match)
      if color then
        table.insert(results, { range = { col_s = s - 1, col_e = e }, color = color })
      end
      pos = e + 1
    end
  end
  table.sort(results, function(a, b) return a.range.col_s < b.range.col_s end)
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
