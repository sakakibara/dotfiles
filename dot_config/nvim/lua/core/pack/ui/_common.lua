local M = {}

M.NS = vim.api.nvim_create_namespace("core.pack.ui")

M.STATUS_MARKED   = "[x]"
M.STATUS_UNMARKED = "[ ]"

M.keymaps = {
  update_review = {
    toggle = "<Tab>", toggle_alt = "x",
    mark_all = "a", mark_none = "u",
    expand = "<C-d>",
    apply = "<CR>", cancel = "q",
  },
  clean_review = {
    toggle = "<Tab>", toggle_alt = "x",
    mark_all = "a", mark_none = "u",
    apply = "<CR>", cancel = "q",
  },
  rollback_review = {
    select = "<CR>", cancel = "q",
  },
  status = {
    cancel = "q",
  },
}

-- Apply window options for pack scratch buffers and lock them against mode-change
-- autocmds elsewhere in the user's config (e.g., toggling relativenumber on InsertEnter).
-- The buffer-local autocmd reasserts these options on BufEnter/WinEnter/ModeChanged.
function M.lock_pack_window(buf, win)
  local function apply()
    if win == nil or not vim.api.nvim_win_is_valid(win) then return end
    vim.wo[win].number = false
    vim.wo[win].relativenumber = false
    vim.wo[win].statuscolumn = ""
    vim.wo[win].signcolumn = "no"
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
    -- Window-local "" actually disables the bar (no line allocated).
    -- The global `winbar = "%!..."` would otherwise leave a blank line
    -- here even though Lib.winbar.render() returns "" for nofile.
    vim.wo[win].winbar = ""
  end
  apply()

  local group = vim.api.nvim_create_augroup("CorePackWindowLock_" .. buf, { clear = true })
  vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter", "ModeChanged" }, {
    group = group,
    buffer = buf,
    callback = function()
      local w = vim.api.nvim_get_current_win()
      if vim.api.nvim_win_get_buf(w) ~= buf then return end
      vim.wo[w].number = false
      vim.wo[w].relativenumber = false
      vim.wo[w].statuscolumn = ""
      vim.wo[w].signcolumn = "no"
      vim.wo[w].wrap = false
      vim.wo[w].cursorline = true
      vim.wo[w].winbar = ""
    end,
  })
end

-- Decide which columns to include given available width.
-- min_widths: { col → byte width }
-- priorities: { col → integer (higher = keep longer) }
-- win_w: total width budget (e.g., from nvim_win_get_width)
-- Returns: { col → bool included }. Always tries to keep at least the highest-priority col.
function M.plan_columns(min_widths, priorities, win_w)
  local include = {}
  local SEP = 2  -- two-space separator between columns
  for k in pairs(min_widths) do include[k] = true end

  local function total()
    local t = 0
    local n = 0
    for k, w in pairs(min_widths) do
      if include[k] then t = t + w; n = n + 1 end
    end
    return t + math.max(0, n - 1) * SEP
  end

  while total() > win_w do
    local victim, victim_pri = nil, math.huge
    for k, p in pairs(priorities) do
      if include[k] and p < victim_pri then victim, victim_pri = k, p end
    end
    if not victim then break end  -- nothing left to drop
    include[victim] = false
  end

  return include
end

-- Pad a glyph to a consistent display width across rows. Some glyphs (●/⚙/◯,
-- emoji-presentation chars) render at different cell widths depending on
-- terminal/font; columns misalign without padding.
function M.pad_glyph(g)
  local w = vim.fn.strdisplaywidth(g)
  if w >= 2 then return g end
  return g .. " "  -- pad to 2 cells
end

function M.format_subject(s, max)
  s = s or ""
  if #s <= max then return s end
  return s:sub(1, max - 1) .. "…"
end

function M.fmt_size_kb(kb)
  if kb < 1024 then return ("%d KB"):format(kb) end
  if kb < 1024 * 1024 then return ("%.1f MB"):format(kb / 1024) end
  return ("%.2f GB"):format(kb / 1024 / 1024)
end

function M.format_ago(ts)
  local age_s = os.time() - (ts or 0)
  if age_s < 60        then return ("%ds ago"):format(age_s) end
  if age_s < 3600      then return ("%dm ago"):format(math.floor(age_s / 60)) end
  if age_s < 86400     then return ("%dh ago"):format(math.floor(age_s / 3600)) end
  return ("%dd ago"):format(math.floor(age_s / 86400))
end

return M
