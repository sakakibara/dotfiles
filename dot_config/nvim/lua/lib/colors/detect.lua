-- lua/lib/colors/detect.lua
-- Detects color literals in a buffer's visible region.
-- Hybrid: TS dispatcher (Task 15) + regex fallback. Hard caps prevent
-- runaway scans (per-line and total-viewport).
local P = require("lib.colors.parse")
local M = {}

M.MAX_LINE_LEN     = 1000
M.MAX_VIEWPORT_KB  = 256

-- detect(buf, top_line, bot_line) → list of { lnum, col_s, col_e, color }.
-- Lines are 0-indexed, both ends inclusive.
function M.detect(buf, top, bot)
  if not vim.api.nvim_buf_is_valid(buf) then return {} end
  local lines = vim.api.nvim_buf_get_lines(buf, top, bot + 1, false)

  -- Total-bytes cap (cheap upper bound: count without skipping).
  local total = 0
  for _, l in ipairs(lines) do total = total + #l end
  if total > M.MAX_VIEWPORT_KB * 1024 then return {} end

  -- Per-line cap + regex scan. TS dispatcher comes in Task 15.
  local out = {}
  for i, line in ipairs(lines) do
    if #line <= M.MAX_LINE_LEN then
      for _, r in ipairs(P.parse_all(line)) do
        table.insert(out, {
          lnum  = top + i - 1,
          col_s = r.range.col_s,
          col_e = r.range.col_e,
          color = r.color,
        })
      end
    end
  end
  return out
end

return M
