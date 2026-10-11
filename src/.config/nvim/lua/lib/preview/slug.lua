local M = {}

local drop_ranges = {
  { 0x80, 0xA9 }, { 0xAB, 0xB1 }, { 0xB4, 0xB4 }, { 0xB6, 0xB8 }, { 0xBB, 0xBB }, { 0xBF, 0xBF },
  { 0xD7, 0xD7 }, { 0xF7, 0xF7 },
  { 0x2000, 0x203E }, { 0x2041, 0x2053 }, { 0x2055, 0x206F },
  { 0x20A0, 0x20CF },
  { 0x2190, 0x245F }, { 0x2500, 0x2775 }, { 0x2794, 0x2BFF },
  { 0x2E00, 0x2E7F },
  { 0x3000, 0x3004 }, { 0x3008, 0x3020 }, { 0x3030, 0x3030 }, { 0x303D, 0x303F },
  { 0x30FB, 0x30FB },
  { 0xFE10, 0xFE19 }, { 0xFE30, 0xFE32 }, { 0xFE35, 0xFE4C }, { 0xFE50, 0xFE6B },
  { 0xFEFF, 0xFEFF },
  { 0xFF01, 0xFF0F }, { 0xFF1A, 0xFF20 }, { 0xFF3B, 0xFF3E }, { 0xFF40, 0xFF40 }, { 0xFF5B, 0xFF65 },
  { 0xFFE0, 0xFFEE },
  { 0x1F000, 0x1FAFF },
}

local function dropped(cp)
  for _, r in ipairs(drop_ranges) do
    if cp < r[1] then return false end
    if cp <= r[2] then return true end
  end
  return false
end

function M.slug(text)
  if not text:find("[\128-\255]") then
    return (text:lower():gsub("[^%w_%- ]", ""):gsub(" ", "-"))
  end
  local lower = vim.fn.tolower(text)
  local out = {}
  for _, cp in ipairs(vim.fn.str2list(lower, true)) do
    if cp < 0x80 then
      local ch = string.char(cp)
      if ch == " " then
        out[#out + 1] = "-"
      elseif ch:match("[%w_%-]") then
        out[#out + 1] = ch
      end
    elseif not dropped(cp) then
      out[#out + 1] = vim.fn.nr2char(cp, true)
    end
  end
  return table.concat(out)
end

function M.new()
  local seen = {}
  return function(text)
    local base = M.slug(text)
    local result = base
    while seen[result] do
      seen[base] = seen[base] + 1
      result = base .. "-" .. seen[base]
    end
    seen[result] = 0
    return result
  end
end

return M
