local M = {}

local STATUS_GLYPH = { pending = "[ ]", ok = "[✓]", error = "[✗]", working = "[…]" }

local NS = vim.api.nvim_create_namespace("core.pack.ui")

local STATUS_MARKED   = "●"
local STATUS_UNMARKED = "◯"

-- progress({ "name1", "name2", ... }, opts)
-- opts: { open_window = bool (default true), title = string }
function M.progress(names, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  pcall(vim.api.nvim_buf_set_name, buf, opts.title or "core.pack: install")

  local lines = { ("Installing %d plugins..."):format(#names) }
  local row_of = {}
  for i, name in ipairs(names) do
    lines[#lines + 1] = ("  %s %s "):format(STATUS_GLYPH.pending, name)
    row_of[name] = i + 1  -- 1-indexed buffer row
  end
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  local win
  if opts.open_window ~= false then
    vim.cmd("botright 12split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
  end

  local view = { buf = buf, win = win }

  function view:set_status(name, status, detail)
    local row = row_of[name]
    if not row then return end
    local glyph = STATUS_GLYPH[status] or STATUS_GLYPH.pending
    local line = ("  %s %s %s"):format(glyph, name, detail or "")
    vim.bo[self.buf].modifiable = true
    vim.api.nvim_buf_set_lines(self.buf, row - 1, row, false, { line })
    vim.bo[self.buf].modifiable = false
  end

  function view:close()
    if self.win and vim.api.nvim_win_is_valid(self.win) then
      vim.api.nvim_win_close(self.win, true)
    end
    if vim.api.nvim_buf_is_valid(self.buf) then
      vim.api.nvim_buf_delete(self.buf, { force = true })
    end
  end

  return view
end

local function format_subject(s, max)
  s = s or ""
  if #s <= max then return s end
  return s:sub(1, max - 1) .. "…"
end

local function update_review_render(buf, pending, marked, expanded)
  local lines = {}
  local marked_count = 0
  for i = 1, #pending do if marked[i] then marked_count = marked_count + 1 end end
  lines[#lines + 1] = ("core.pack: %d of %d marked   <Tab> toggle  a all  u none  <C-d> expand  <CR> apply  q cancel")
    :format(marked_count, #pending)

  local row_to_index = {}  -- buffer row → pending index (for keymap targeting)
  local highlights = {}    -- { { row, col_start, col_end, hl_group }, ... }
  local row = 2  -- 1-indexed; row 1 is header

  for i, p in ipairs(pending) do
    local glyph = marked[i] and STATUS_MARKED or STATUS_UNMARKED
    local hl_glyph = marked[i] and "Special" or "Comment"
    local name = ("%-22s"):format(p.name)
    local range = ("%s..%s"):format(p.from:sub(1, 7), p.to:sub(1, 7))
    local count_str = (p.count == 1) and "1 commit " or (("%d commits"):format(p.count))
    local subject = format_subject(p.subject, 60)
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
        lines[#lines + 1] = ("        %s  %s"):format(sha.sha:sub(1, 7), sha.subject)
        table.insert(highlights, { row - 1, 8, 15, "Comment" })
        table.insert(highlights, { row - 1, 17, 17 + #sha.subject, "Comment" })
        row = row + 1
      end
    end
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  vim.api.nvim_buf_clear_namespace(buf, NS, 0, -1)
  -- Header highlight (line 0).
  vim.api.nvim_buf_set_extmark(buf, NS, 0, 0, { end_col = #lines[1], hl_group = "Title" })
  for _, hl in ipairs(highlights) do
    vim.api.nvim_buf_set_extmark(buf, NS, hl[1], hl[2], { end_col = hl[3], hl_group = hl[4] })
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
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: update review")

  local marked = {}
  local expanded = {}
  for i = 1, #pending do marked[i] = true end

  local row_to_index = update_review_render(buf, pending, marked, expanded)

  local win
  if opts.open_window ~= false then
    vim.cmd("botright 18split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
    vim.api.nvim_win_set_cursor(win, { 2, 0 })
  end

  local view = { buf = buf, win = win }

  function view:toggle_at(row)
    local i = row_to_index[row]
    if not i then return end
    marked[i] = not marked[i]
    row_to_index = update_review_render(buf, pending, marked, expanded)
  end

  function view:set_all(value)
    for i = 1, #pending do marked[i] = value end
    row_to_index = update_review_render(buf, pending, marked, expanded)
  end

  function view:toggle_expand(row)
    local i = row_to_index[row]
    if not i then return end
    if not pending[i]._log and opts.on_expand then
      -- Fetch lazily then re-render.
      opts.on_expand(pending[i].name, function(log)
        pending[i]._log = log or {}
        expanded[i] = true
        row_to_index = update_review_render(buf, pending, marked, expanded)
      end)
      return
    end
    expanded[i] = not expanded[i]
    row_to_index = update_review_render(buf, pending, marked, expanded)
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
    local function map(lhs, rhs) vim.keymap.set("n", lhs, rhs, { buffer = buf, nowait = true, silent = true }) end
    map("<Tab>", function() view:toggle_at(vim.api.nvim_win_get_cursor(win)[1]) end)
    map("x",     function() view:toggle_at(vim.api.nvim_win_get_cursor(win)[1]) end)
    map("a",     function() view:set_all(true) end)
    map("u",     function() view:set_all(false) end)
    map("<C-d>", function() view:toggle_expand(vim.api.nvim_win_get_cursor(win)[1]) end)
    map("<CR>",  function() view:apply(); view:close() end)
    map("q",     function() view:close() end)
  end

  return view
end

-- status(lines, opts) - read-only scratch buffer for displaying status text.
-- opts: { open_window = bool (default true), title = string }
function M.status(lines, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  pcall(vim.api.nvim_buf_set_name, buf, opts.title or "core.pack: status")
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  local win
  if opts.open_window ~= false then
    vim.cmd("botright " .. math.min(math.max(#lines + 2, 8), 24) .. "split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
    vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = buf, nowait = true, silent = true })
  end

  local view = { buf = buf, win = win }
  function view:close()
    if self.win and vim.api.nvim_win_is_valid(self.win) then
      vim.api.nvim_win_close(self.win, true)
    end
    if vim.api.nvim_buf_is_valid(self.buf) then
      vim.api.nvim_buf_delete(self.buf, { force = true })
    end
  end
  return view
end

return M
