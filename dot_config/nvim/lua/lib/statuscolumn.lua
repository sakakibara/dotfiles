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

-- Cursor-scoped fold bar: a thin colored vertical line that spans only the
-- range of the cursor's enclosing fold, with color encoding the fold depth.
-- Off any line that isn't part of the cursor's current fold scope.
--
-- Top of range uses the configured foldopen chevron (Lib.icons.status.FoldOpen,
-- the same glyph the old fold_for column drew); middle is `│`; bottom is `└`
-- (sharp corner, no rounded variant). Closed folds get the foldclose glyph.
-- Scope range is recomputed on CursorMoved per-buffer, then read by render
-- on every line draw — cheap O(1) lookup at draw time.
local FOLD_MID  = "│"
local FOLD_BOT  = "└"
local LEVEL_HLS = { "StcFoldLvl1", "StcFoldLvl2", "StcFoldLvl3", "StcFoldLvl4" }
M._fold_scope = {}  -- { [bufnr] = { start, finish, depth } | nil }

-- Cap each direction of the scope walk so a deep-fold cursor on a giant
-- buffer can't stall the editor. 2000 lines covers anything human-readable;
-- bigger fold scopes than that aren't meaningful as a visual indicator.
local SCOPE_WALK_MAX = 2000

local function update_fold_scope(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    M._fold_scope[buf] = nil
    return
  end
  -- Skip special buffers (terminal, prompt, nofile help/scratch) — they don't
  -- have meaningful folds and several plugins create them during startup,
  -- which is part of the freeze cost we're trying to avoid.
  if vim.bo[buf].buftype ~= "" then
    M._fold_scope[buf] = nil
    return
  end
  local win = vim.api.nvim_get_current_win()
  if vim.api.nvim_win_get_buf(win) ~= buf then return end
  local cur = vim.api.nvim_win_get_cursor(win)[1]
  local depth = vim.fn.foldlevel(cur)
  if depth == 0 then
    M._fold_scope[buf] = nil
    return
  end
  local last = vim.fn.line("$")
  local s, e = cur, cur
  local walked = 0
  while s > 1 and walked < SCOPE_WALK_MAX and vim.fn.foldlevel(s - 1) >= depth do
    s = s - 1
    walked = walked + 1
  end
  walked = 0
  while e < last and walked < SCOPE_WALK_MAX and vim.fn.foldlevel(e + 1) >= depth do
    e = e + 1
    walked = walked + 1
  end
  M._fold_scope[buf] = { start = s, finish = e, depth = depth }
end

-- Defer update to vim.schedule so we don't block the inciting autocmd, and
-- coalesce multiple events fired in the same tick (BufEnter+WinEnter often
-- fire together) into a single computation.
local _pending = {}
local function schedule_update(buf)
  if _pending[buf] then return end
  _pending[buf] = true
  vim.schedule(function()
    _pending[buf] = nil
    update_fold_scope(buf)
  end)
end

local function fold_scope_bar(buf, lnum)
  local scope = M._fold_scope[buf]
  if not scope or lnum < scope.start or lnum > scope.finish then return " " end
  local hl = LEVEL_HLS[math.min(scope.depth, #LEVEL_HLS)]
  local glyph
  if scope.start == scope.finish then
    glyph = Lib.icons.status.FoldClose
  elseif lnum == scope.start then
    glyph = Lib.icons.status.FoldOpen
  elseif lnum == scope.finish then
    glyph = FOLD_BOT
  else
    glyph = FOLD_MID
  end
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
    fold_for(lnum),
    fold_scope_bar(buf, lnum),
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

  -- Track cursor's enclosing fold range so the scope bar follows it.
  -- Only listen to events that genuinely move the cursor across fold
  -- boundaries — TextChanged/TextChangedI fire on every keystroke and would
  -- recompute uselessly for typing inside a fold; BufEnter/WinEnter cover
  -- cursor placement on buffer switch.
  local grp = vim.api.nvim_create_augroup("Lib.statuscolumn.scope", { clear = true })
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter", "WinEnter" }, {
    group = grp,
    callback = function(args) schedule_update(args.buf) end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    group = grp,
    callback = function(args)
      M._fold_scope[args.buf] = nil
      _pending[args.buf] = nil
    end,
  })
end

return M
