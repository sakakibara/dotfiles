-- lua/lib/colors/contrast.lua
-- Render WCAG contrast ratios as virt-text on lines that contain exactly
-- two color literals. Toggleable per buffer via `:ColorContrast` (the
-- command lives in init.lua).
--
-- Pairing scope is per-line: this is the simplest signal that catches
-- the common CSS shape (`background: X; color: Y;` on one line, or two
-- adjacent declarations). For multi-line rule pairing, we'd need
-- selector-aware analysis — out of scope here.

local C = require("lib.colors.color")
local D = require("lib.colors.detect")
local M = {}

M.ns = vim.api.nvim_create_namespace("Lib.colors.contrast")
-- Per-buffer toggle: nil = follow global default (off), true/false = explicit.
M._enabled = {}

-- Format the virt-text payload: ratio + level. Highlight group is chosen
-- by level so the eye can spot a "fail" without parsing the number.
local function virt_text(ratio, level)
  local hl = level == "AAA" and "DiagnosticOk"
          or level == "AA"  and "DiagnosticHint"
          or                    "DiagnosticError"
  local txt = string.format(" %.1f:1 %s", ratio, level)
  return { { txt, hl } }
end

-- Public: render contrast marks for the lines [top, bot] of `buf`. Skips
-- lines that don't have exactly two distinct color literals.
--
-- `hits` is optional — when supplied (e.g. from the main render path that
-- already ran D.detect), we re-bucket them instead of re-detecting. Saves
-- a second TS query pass per redraw when contrast hints are on.
function M.render(buf, top, bot, hits)
  vim.api.nvim_buf_clear_namespace(buf, M.ns, top, bot + 1)
  if not vim.api.nvim_buf_is_valid(buf) then return end

  hits = hits or D.detect(buf, top, bot)
  -- Bucket hits by lnum.
  local by_line = {}
  for _, h in ipairs(hits) do
    by_line[h.lnum] = by_line[h.lnum] or {}
    table.insert(by_line[h.lnum], h)
  end

  for lnum, line_hits in pairs(by_line) do
    if #line_hits == 2 then
      local c1, c2 = line_hits[1].color, line_hits[2].color
      -- Skip when both are the same color (ratio 1.0 isn't useful).
      if not (c1.r == c2.r and c1.g == c2.g and c1.b == c2.b) then
        local ratio = C.contrast_ratio(c1, c2)
        local level = C.contrast_level(ratio)
        vim.api.nvim_buf_set_extmark(buf, M.ns, lnum, 0, {
          virt_text     = virt_text(ratio, level),
          virt_text_pos = "eol",
          hl_mode       = "combine",
        })
      end
    end
  end
end

function M.clear(buf)
  if vim.api.nvim_buf_is_valid(buf) then
    vim.api.nvim_buf_clear_namespace(buf, M.ns, 0, -1)
  end
end

-- Toggle per-buffer. Returns the new enabled state.
function M.toggle(buf)
  if not buf or buf == 0 then buf = vim.api.nvim_get_current_buf() end
  M._enabled[buf] = not M._enabled[buf]
  if M._enabled[buf] then
    -- Render immediately for the visible viewport.
    local top = 0
    local bot = math.max(0, vim.api.nvim_buf_line_count(buf) - 1)
    M.render(buf, top, bot)
  else
    M.clear(buf)
  end
  return M._enabled[buf]
end

function M.is_enabled(buf) return M._enabled[buf] == true end

return M
