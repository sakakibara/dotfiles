-- lua/lib/unicode.lua
-- Display-width-aware string slicing. Lua's `string.sub` and `#str` work
-- on bytes; for CJK / emoji / any multi-byte content they split mid
-- codepoint and produce orphan continuation bytes that nvim renders as
-- `<89>` etc. These helpers truncate by display columns (the unit
-- statuslines and labels actually care about).

local M = {}

local strchars        = vim.fn.strchars
local strcharpart     = vim.fn.strcharpart
local strdisplaywidth = vim.fn.strdisplaywidth

function M.width(str)
  return strdisplaywidth(str)
end

-- Longest prefix of `str` whose display width is <= `max_w`. CJK chars
-- (display width 2) are kept whole; never split mid-codepoint.
function M.head(str, max_w)
  if max_w <= 0 then return "" end
  if strdisplaywidth(str) <= max_w then return str end
  local out, w = "", 0
  for i = 0, strchars(str) - 1 do
    local c  = strcharpart(str, i, 1)
    local cw = strdisplaywidth(c)
    if w + cw > max_w then break end
    out = out .. c
    w = w + cw
  end
  return out
end

-- Longest suffix of `str` whose display width is <= `max_w`.
function M.tail(str, max_w)
  if max_w <= 0 then return "" end
  if strdisplaywidth(str) <= max_w then return str end
  local out, w = "", 0
  for i = strchars(str) - 1, 0, -1 do
    local c  = strcharpart(str, i, 1)
    local cw = strdisplaywidth(c)
    if w + cw > max_w then break end
    out = c .. out
    w = w + cw
  end
  return out
end

return M
