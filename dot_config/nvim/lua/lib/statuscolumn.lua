-- lua/lib/statuscolumn.lua
local M = {}

local function gitsign_for(buf, lnum)
  local gs = vim.fn.sign_getplaced(buf, { group = "gitsigns_vimfn_signs_", lnum = lnum })[1]
  if not gs or not gs.signs or #gs.signs == 0 then return " " end
  local sign = gs.signs[1]
  local def = vim.fn.sign_getdefined(sign.name)[1]
  if def and def.text then
    return "%#" .. (def.texthl or "GitSignsAdd") .. "#" .. def.text .. "%*"
  end
  return " "
end

local function number_for(lnum, relnum, virtnum)
  if virtnum and virtnum ~= 0 then return "    " end
  local n
  if vim.wo.relativenumber and relnum and relnum ~= 0 then n = relnum
  else n = lnum end
  return string.format("%4d", n)
end

-- Single-column fold indicator: a thin vertical bar whose color encodes the
-- fold depth (level 1..4 → distinct hl groups, deeper levels reuse the
-- bottom group). Closed folds render `▶` instead of the bar so they stand
-- out at a glance. Empty space when the line isn't inside any fold.
local FOLD_BAR    = "▏"  -- LEFT ONE EIGHTH BLOCK
local FOLD_CLOSED = "▶"
local LEVEL_HLS   = { "StcFoldLvl1", "StcFoldLvl2", "StcFoldLvl3", "StcFoldLvl4" }

local function fold_bar(lnum)
  local lvl = vim.fn.foldlevel(lnum)
  if lvl == 0 then return " " end
  local hl = LEVEL_HLS[math.min(lvl, #LEVEL_HLS)]
  local glyph = (vim.fn.foldclosed(lnum) > 0) and FOLD_CLOSED or FOLD_BAR
  return "%#" .. hl .. "#" .. glyph .. "%*"
end

function M.render()
  local lnum = vim.v.lnum
  local relnum = vim.v.relnum
  local virtnum = vim.v.virtnum
  local buf = vim.api.nvim_get_current_buf()

  return table.concat({
    "%s",  -- signs column (neovim handles this)
    number_for(lnum, relnum, virtnum),
    " ",
    gitsign_for(buf, lnum),
    fold_bar(lnum),
    " ",
  })
end

local function get(group, attr, fallback)
  local h = vim.api.nvim_get_hl(0, { name = group, link = false })
  local val = h[attr]
  if val == nil then return fallback end
  return ("#%06x"):format(val)
end

local function define_highlights()
  -- Cycle through semantic groups so the bar is theme-aware. The order
  -- (Function/String/Type/Constant) gives a noticeable hue progression
  -- that doesn't clash with line numbers or gitsigns.
  vim.api.nvim_set_hl(0, "StcFoldLvl1", { fg = get("Function", "fg", "#89b4fa") })
  vim.api.nvim_set_hl(0, "StcFoldLvl2", { fg = get("String",   "fg", "#a6e3a1") })
  vim.api.nvim_set_hl(0, "StcFoldLvl3", { fg = get("Type",     "fg", "#f9e2af") })
  vim.api.nvim_set_hl(0, "StcFoldLvl4", { fg = get("Constant", "fg", "#fab387") })
end

function M.setup()
  define_highlights()
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("Lib.statuscolumn.hl", { clear = true }),
    callback = function() pcall(define_highlights) end,
  })
end

return M
