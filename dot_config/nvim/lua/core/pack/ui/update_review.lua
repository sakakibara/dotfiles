local C = require("core.pack.ui._common")
local M = {}

local function render(buf, win, pending, marked, expanded)
  local lines = {}
  local marked_count = 0
  for i = 1, #pending do if marked[i] then marked_count = marked_count + 1 end end
  lines[#lines + 1] = ("core.pack: %d of %d marked   <Tab> toggle  a all  u none  <C-d> expand  <CR> apply  q cancel")
    :format(marked_count, #pending)

  local row_to_index = {}  -- buffer row → pending index (for keymap targeting)
  local highlights = {}    -- { { row, col_start, col_end, hl_group }, ... }

  -- Name column: longest actual name, floor 16, cap 50.
  local name_max = 16
  for _, p in ipairs(pending) do name_max = math.max(name_max, #p.name) end
  if name_max > 50 then name_max = 50 end

  -- Bytes before subject: 2 (lead) + 3 (glyph) + 2 + name_max + 2 + 17 (range) + 2 + 11 (count) + 2
  local PREFIX_BYTES = 2 + 3 + 2 + name_max + 2 + 17 + 2 + 11 + 2
  local subject_max
  if win and vim.api.nvim_win_is_valid(win) then
    subject_max = math.max(20, vim.api.nvim_win_get_width(win) - PREFIX_BYTES)
  else
    subject_max = 200  -- headless tests: wide enough not to truncate
  end

  local row = 2  -- 1-indexed; row 1 is header

  for i, p in ipairs(pending) do
    local glyph = C.pad_glyph(marked[i] and C.STATUS_MARKED or C.STATUS_UNMARKED)
    local hl_glyph = marked[i] and "Special" or "NonText"
    local name_truncated = p.name
    if #name_truncated > name_max then name_truncated = name_truncated:sub(1, name_max - 1) .. "…" end
    local name = ("%-" .. name_max .. "s"):format(name_truncated)
    local range = ("%s..%s"):format(p.from:sub(1, 7), p.to:sub(1, 7))
    local count_str = (p.count == 1) and "1 commit " or (("%d commits"):format(p.count))
    local subj_full = p.subject or ""
    if p.ago and p.ago ~= "" then subj_full = subj_full .. " (" .. p.ago .. ")" end
    local subject = C.format_subject(subj_full, subject_max)
    local line = ("  %s  %s  %s  %s  %s"):format(glyph, name, range, count_str, subject)
    lines[#lines + 1] = line
    row_to_index[row] = i

    -- Compute column ranges for highlighting.
    local col = 2  -- two leading spaces
    table.insert(highlights, { row - 1, col, col + #glyph, hl_glyph }); col = col + #glyph + 2
    table.insert(highlights, { row - 1, col, col + #name, "Identifier" }); col = col + #name + 2
    -- range halves
    table.insert(highlights, { row - 1, col, col + 7, "DiffRemoved" })
    table.insert(highlights, { row - 1, col + 9, col + 9 + 7, "DiffAdded" })
    col = col + #range + 2
    table.insert(highlights, { row - 1, col, col + #count_str, "Number" }); col = col + #count_str + 2
    table.insert(highlights, { row - 1, col, col + #subject, "Comment" })
    row = row + 1

    if expanded[i] and p._log then
      for _, sha in ipairs(p._log) do
        local meta = ""
        if sha.author and sha.author ~= "" then
          meta = ("  %s, %s"):format(sha.author, sha.ago or "")
        end
        lines[#lines + 1] = ("        %s  %s%s"):format(sha.sha:sub(1, 7), sha.subject, meta)
        table.insert(highlights, { row - 1, 8, 15, "Comment" })
        table.insert(highlights, { row - 1, 17, 17 + #sha.subject, "Comment" })
        if meta ~= "" then
          table.insert(highlights, { row - 1, 17 + #sha.subject, 17 + #sha.subject + #meta, "DiagnosticHint" })
        end
        row = row + 1
      end
    end
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  vim.api.nvim_buf_clear_namespace(buf, C.NS, 0, -1)
  -- Header highlight (line 0).
  vim.api.nvim_buf_set_extmark(buf, C.NS, 0, 0, { end_col = #lines[1], hl_group = "Title" })
  for _, hl in ipairs(highlights) do
    vim.api.nvim_buf_set_extmark(buf, C.NS, hl[1], hl[2], { end_col = hl[3], hl_group = hl[4] })
  end
  return row_to_index
end

-- update_review(pending, opts)
-- pending: { { name, from, to, count, subject, dir, ... }, ... }
-- opts: { open_window = bool, on_apply = function(marked_entries) end, on_close = function() end }
function M.update_review(pending, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  if opts.open_window == false then vim.bo[buf].filetype = "PackReview" end
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: update review")

  local marked = {}
  local expanded = {}
  for i = 1, #pending do marked[i] = true end

  local win  -- declared early so render closures can read its width

  local row_to_index = render(buf, win, pending, marked, expanded)

  if opts.open_window ~= false then
    vim.cmd("topleft 18split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.bo[buf].filetype = "PackReview"  -- after win_set_buf so ftdetect doesn't clear
    C.lock_pack_window(buf, win)
    vim.api.nvim_win_set_cursor(win, { 2, 0 })
    -- Force normal mode in case user was in insert in another buffer.
    -- modifiable=false alone doesn't help if the user is already mid-insert.
    vim.cmd("stopinsert")
    vim.api.nvim_create_autocmd("InsertEnter", {
      buffer = buf,
      callback = function() vim.cmd("stopinsert") end,
    })
    -- Re-render now that we have the real window width for subject truncation.
    row_to_index = render(buf, win, pending, marked, expanded)
  end

  local view = { buf = buf, win = win }

  function view:toggle_at(row)
    local i = row_to_index[row]
    if not i then return end
    marked[i] = not marked[i]
    row_to_index = render(buf, win, pending, marked, expanded)
  end

  function view:set_all(value)
    for i = 1, #pending do marked[i] = value end
    row_to_index = render(buf, win, pending, marked, expanded)
  end

  function view:toggle_expand(row)
    local i = row_to_index[row]
    if not i then return end
    if not pending[i]._log and opts.on_expand then
      -- Fetch lazily then re-render.
      opts.on_expand(pending[i].name, function(log)
        pending[i]._log = log or {}
        expanded[i] = true
        row_to_index = render(buf, win, pending, marked, expanded)
      end)
      return
    end
    expanded[i] = not expanded[i]
    row_to_index = render(buf, win, pending, marked, expanded)
  end

  function view:apply()
    local out = {}
    for i, p in ipairs(pending) do if marked[i] then out[#out + 1] = p end end
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
    local km = vim.tbl_extend("force", C.keymaps.update_review, opts.keymaps or {})
    local function map(lhs, rhs) vim.keymap.set("n", lhs, rhs, { buffer = buf, nowait = true, silent = true }) end
    map(km.toggle,     function() view:toggle_at(vim.api.nvim_win_get_cursor(win)[1]) end)
    map(km.toggle_alt, function() view:toggle_at(vim.api.nvim_win_get_cursor(win)[1]) end)
    map(km.mark_all,   function() view:set_all(true) end)
    map(km.mark_none,  function() view:set_all(false) end)
    map(km.expand,     function() view:toggle_expand(vim.api.nvim_win_get_cursor(win)[1]) end)
    map(km.apply,      function() view:apply(); view:close() end)
    map(km.cancel,     function() view:close() end)
    -- Block edit-mode entry keys; the buffer is modifiable=false but the brief
    -- modifiable window during re-render can let queued insert keys through if
    -- the user was typing in another buffer when the review window opened.
    for _, k in ipairs({ "i", "I", "A", "o", "O", "c", "C", "s", "S", "r", "R", "p", "P" }) do
      vim.keymap.set("n", k, "<nop>", { buffer = buf, nowait = true, silent = true })
    end
  end

  return view
end

return M
