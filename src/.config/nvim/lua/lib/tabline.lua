-- Feature-rich tabline — for actual tab pages, not buffers. Shows:
--   [N] <icon> <focus-buffer-name> +<extra-window-count> <modified-dot>
-- Clickable tab pills (left-click switches, middle-click closes via %NX),
-- cwd on the right, inherits TabLineSel / TabLine / TabLineFill from the
-- colorscheme so it looks native across themes.
--
-- Only renders when showtabline=1 and more than one tab page exists.

local M = {}

local function buf_icon(buf, base_hl)
  local ok, devicons = pcall(require, "nvim-web-devicons")
  if not ok then return "" end
  local name = vim.api.nvim_buf_get_name(buf)
  local ext  = vim.fn.fnamemodify(name, ":e")
  local icon, icon_hl = devicons.get_icon(name, ext, { default = true })
  if not icon then return "" end
  -- Switch to icon hl for the glyph, then back to base_hl for following text
  return ("%%#%s#%s %%#%s#"):format(icon_hl or base_hl, icon, base_hl)
end

local function tab_info(tab)
  local wins       = vim.api.nvim_tabpage_list_wins(tab)
  local focus_win  = vim.api.nvim_tabpage_get_win(tab)
  local focus_buf  = vim.api.nvim_win_get_buf(focus_win)
  local name       = vim.api.nvim_buf_get_name(focus_buf)
  local label      = name == "" and "[No Name]" or vim.fn.fnamemodify(name, ":t")
  local any_mod    = false
  for _, w in ipairs(wins) do
    if vim.bo[vim.api.nvim_win_get_buf(w)].modified then any_mod = true; break end
  end
  return {
    buf      = focus_buf,
    label    = label,
    wins     = #wins,
    modified = any_mod,
  }
end

-- strwidth of a label plus fixed per-tab decoration: 3-char number pill
-- ("  N  "), optional icon (2 cells), 2-space pre-label padding,
-- optional +K wincount (3 cells), optional modified marker (3 cells),
-- trailing 2 spaces. We track the "decoration overhead" per tab so we
-- know how much the label can take from the remaining budget.
local function decoration_width(info)
  local w = 5                                   -- "  N  " (approx — N may be >9)
  w = w + 2                                     -- icon + space
  if info.wins > 1 then w = w + 3 end           -- "  +K"
  if info.modified then w = w + 3 end           -- "  ●"
  w = w + 2                                     -- trailing "  "
  return w
end

function M.render()
  local tabs    = vim.api.nvim_list_tabpages()
  local current = vim.api.nvim_get_current_tabpage()

  -- Right side: cwd, dimmed. Computed first so we can subtract from budget.
  local cwd      = vim.fn.fnamemodify(vim.fn.getcwd(), ":~")
  local cwd_text = " " .. Lib.icons.status.Directory .. cwd .. " "
  local cwd_w    = vim.fn.strdisplaywidth(cwd_text)

  -- Compute full decoration + label widths per tab for budget math.
  local cells = {}
  local total_needed = 0
  for _, tab in ipairs(tabs) do
    local info = tab_info(tab)
    local deco = decoration_width(info)
    local lab  = vim.fn.strdisplaywidth(info.label)
    cells[#cells + 1] = {
      tab  = tab,
      info = info,
      deco = deco,
      lab  = lab,
    }
    total_needed = total_needed + deco + lab
  end

  local columns = vim.o.columns
  local tab_budget = columns - cwd_w - 1
  local label_budget = tab_budget

  -- How much room do labels together get? Decoration is fixed; labels
  -- collapse first. If still not enough, we have to truncate labels
  -- proportionally.
  local total_deco = 0
  for _, c in ipairs(cells) do total_deco = total_deco + c.deco end
  local total_lab_allowed = math.max(0, tab_budget - total_deco)

  -- Distribute proportionally to each cell's original label width.
  local total_lab_natural = 0
  for _, c in ipairs(cells) do total_lab_natural = total_lab_natural + c.lab end

  local parts = {}
  for _, c in ipairs(cells) do
    local is_cur = c.tab == current
    local hl     = is_cur and "TabLineSel" or "TabLine"
    local idx    = vim.api.nvim_tabpage_get_number(c.tab)

    -- Target width for this tab's label
    local label_shown = c.info.label
    if total_lab_natural > total_lab_allowed and total_lab_natural > 0 then
      local share = math.max(3, math.floor(c.lab * total_lab_allowed / total_lab_natural))
      if share < c.lab then
        if share >= 4 then
          label_shown = Lib.unicode.head(label_shown, share - 1) .. "…"
        else
          label_shown = Lib.unicode.head(label_shown, math.max(1, share))
        end
      end
    end

    local pieces = {
      ("%%%dT"):format(idx),
      "%#" .. hl .. "#",
      ("  %d  "):format(idx),
      buf_icon(c.info.buf, hl),
      label_shown,
    }
    if c.info.wins > 1 then
      pieces[#pieces + 1] = ("  +%d"):format(c.info.wins - 1)
    end
    if c.info.modified then
      pieces[#pieces + 1] = "  " .. Lib.icons.git.modified
    end
    pieces[#pieces + 1] = "  %T"

    parts[#parts + 1] = table.concat(pieces)
  end

  local right = "%#TabLineFill#%=" .. "%#TabLine#" .. cwd_text
  return table.concat(parts, "") .. right
end

function M.setup()
  -- Re-render on VimResized so per-tab labels reflow immediately when
  -- the terminal is resized.
  vim.api.nvim_create_autocmd({ "VimResized" }, {
    group = vim.api.nvim_create_augroup("Lib.tabline.redraw", { clear = true }),
    callback = function() pcall(vim.cmd, "redrawtabline") end,
  })
end

return M
