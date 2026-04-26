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
