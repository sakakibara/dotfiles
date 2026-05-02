local C = require("core.pack.ui._common")
local M = {}

local function render(buf, win, items, marked)
  local lines = {}
  local marked_count = 0
  local total_kb = 0
  for i = 1, #items do
    if marked[i] then marked_count = marked_count + 1 end
    total_kb = total_kb + (items[i].size_kb or 0)
  end
  local header = ("core.pack: %d of %d marked   %s total   <Tab> toggle  a all  u none  <CR> remove  q cancel")
    :format(marked_count, #items, C.fmt_size_kb(total_kb))

  local row_to_index = {}
  local highlights = {}
  local row = 1

  -- Name column: longest actual name, floor 16, cap 50.
  local name_max = 16
  for _, p in ipairs(items) do name_max = math.max(name_max, #p.name) end
  if name_max > 50 then name_max = 50 end

  for i, p in ipairs(items) do
    local glyph = C.pad_glyph(marked[i] and C.STATUS_MARKED or C.STATUS_UNMARKED)
    local hl_glyph = marked[i] and "Special" or "NonText"
    local name_truncated = p.name
    if #name_truncated > name_max then name_truncated = name_truncated:sub(1, name_max - 1) .. "…" end
    local name = ("%-" .. name_max .. "s"):format(name_truncated)
    local size_str = C.fmt_size_kb(p.size_kb or 0)
    local line = ("  %s  %s  %s"):format(glyph, name, size_str)
    lines[#lines + 1] = line
    row_to_index[row] = i

    local col = 2
    table.insert(highlights, { row - 1, col, col + #glyph, hl_glyph }); col = col + #glyph + 2
    table.insert(highlights, { row - 1, col, col + #name, "Identifier" }); col = col + #name + 2
    table.insert(highlights, { row - 1, col, col + #size_str, "Number" })
    row = row + 1
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.api.nvim_buf_clear_namespace(buf, C.NS, 0, -1)
  for _, hl in ipairs(highlights) do
    vim.api.nvim_buf_set_extmark(buf, C.NS, hl[1], hl[2], { end_col = hl[3], hl_group = hl[4] })
  end

  if win and vim.api.nvim_win_is_valid(win) then
    vim.wo[win].winbar = "%#Title#" .. header:gsub("%%", "%%%%")
  end

  return row_to_index, header
end

function M.clean_review(items, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  vim.b[buf].lib_winbar_keep = true
  if opts.open_window == false then vim.bo[buf].filetype = "PackClean" end
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: clean review")

  local marked = {}
  for i = 1, #items do marked[i] = true end

  local win  -- declared early so render closures can read its width
  local row_to_index, header = render(buf, nil, items, marked)

  if opts.open_window ~= false then
    vim.cmd("topleft 14split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.bo[buf].filetype = "PackClean"  -- after win_set_buf so ftdetect doesn't clear
    C.lock_pack_window(buf, win)
    vim.api.nvim_win_set_cursor(win, { 1, 0 })
    vim.cmd("stopinsert")
    vim.api.nvim_create_autocmd("InsertEnter", {
      buffer = buf,
      callback = function() vim.cmd("stopinsert") end,
    })
    -- Re-render now that we have the real window width for dir truncation.
    row_to_index, header = render(buf, win, items, marked)
  end

  local view = { buf = buf, win = win, header = header }

  function view:toggle_at(row)
    local i = row_to_index[row]
    if not i then return end
    marked[i] = not marked[i]
    row_to_index, self.header = render(buf, win, items, marked)
  end

  function view:set_all(value)
    for i = 1, #items do marked[i] = value end
    row_to_index, self.header = render(buf, win, items, marked)
  end

  function view:apply()
    local out = {}
    for i, p in ipairs(items) do if marked[i] then out[#out + 1] = p end end
    if opts.on_apply then opts.on_apply(out) end
  end

  function view:close()
    if opts.on_close then opts.on_close() end
    if self.win and vim.api.nvim_win_is_valid(self.win) then
      vim.api.nvim_win_close(self.win, true)
    end
    if vim.api.nvim_buf_is_valid(self.buf) then
      vim.api.nvim_buf_delete(self.buf, { force = true })
    end
  end

  if win then
    local km = vim.tbl_extend("force", C.keymaps.clean_review, opts.keymaps or {})
    local function map(lhs, rhs) vim.keymap.set("n", lhs, rhs, { buffer = buf, nowait = true, silent = true }) end
    map(km.toggle,     function() view:toggle_at(vim.api.nvim_win_get_cursor(win)[1]) end)
    map(km.toggle_alt, function() view:toggle_at(vim.api.nvim_win_get_cursor(win)[1]) end)
    map(km.mark_all,   function() view:set_all(true) end)
    map(km.mark_none,  function() view:set_all(false) end)
    map(km.apply,      function() view:apply(); view:close() end)
    map(km.cancel,     function() view:close() end)
    for _, k in ipairs({ "i", "I", "A", "o", "O", "c", "C", "s", "S", "r", "R", "p", "P" }) do
      vim.keymap.set("n", k, "<nop>", { buffer = buf, nowait = true, silent = true })
    end
  end

  return view
end

return M
