local M = {}

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

local NS = vim.api.nvim_create_namespace("core.pack.ui")

local STATUS_MARKED   = "●"
local STATUS_UNMARKED = "◯"

local function format_subject(s, max)
  s = s or ""
  if #s <= max then return s end
  return s:sub(1, max - 1) .. "…"
end

local function update_review_render(buf, win, pending, marked, expanded)
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
    local glyph = marked[i] and STATUS_MARKED or STATUS_UNMARKED
    local hl_glyph = marked[i] and "Special" or "Comment"
    local name_truncated = p.name
    if #name_truncated > name_max then name_truncated = name_truncated:sub(1, name_max - 1) .. "…" end
    local name = ("%-" .. name_max .. "s"):format(name_truncated)
    local range = ("%s..%s"):format(p.from:sub(1, 7), p.to:sub(1, 7))
    local count_str = (p.count == 1) and "1 commit " or (("%d commits"):format(p.count))
    local subj_full = p.subject or ""
    if p.ago and p.ago ~= "" then subj_full = subj_full .. " (" .. p.ago .. ")" end
    local subject = format_subject(subj_full, subject_max)
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
  if opts.open_window == false then vim.bo[buf].filetype = "PackReview" end
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: update review")

  local marked = {}
  local expanded = {}
  for i = 1, #pending do marked[i] = true end

  local win  -- declared early so render closures can read its width

  local row_to_index = update_review_render(buf, win, pending, marked, expanded)

  if opts.open_window ~= false then
    vim.cmd("topleft 18split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.bo[buf].filetype = "PackReview"  -- after win_set_buf so ftdetect doesn't clear
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
    vim.wo[win].number = false
    vim.wo[win].relativenumber = false
    vim.wo[win].statuscolumn = ""
    vim.wo[win].signcolumn = "no"
    vim.api.nvim_win_set_cursor(win, { 2, 0 })
    -- Force normal mode in case user was in insert in another buffer.
    -- modifiable=false alone doesn't help if the user is already mid-insert.
    vim.cmd("stopinsert")
    vim.api.nvim_create_autocmd("InsertEnter", {
      buffer = buf,
      callback = function() vim.cmd("stopinsert") end,
    })
    -- Re-render now that we have the real window width for subject truncation.
    row_to_index = update_review_render(buf, win, pending, marked, expanded)
  end

  local view = { buf = buf, win = win }

  function view:toggle_at(row)
    local i = row_to_index[row]
    if not i then return end
    marked[i] = not marked[i]
    row_to_index = update_review_render(buf, win, pending, marked, expanded)
  end

  function view:set_all(value)
    for i = 1, #pending do marked[i] = value end
    row_to_index = update_review_render(buf, win, pending, marked, expanded)
  end

  function view:toggle_expand(row)
    local i = row_to_index[row]
    if not i then return end
    if not pending[i]._log and opts.on_expand then
      -- Fetch lazily then re-render.
      opts.on_expand(pending[i].name, function(log)
        pending[i]._log = log or {}
        expanded[i] = true
        row_to_index = update_review_render(buf, win, pending, marked, expanded)
      end)
      return
    end
    expanded[i] = not expanded[i]
    row_to_index = update_review_render(buf, win, pending, marked, expanded)
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
    local km = vim.tbl_extend("force", M.keymaps.update_review, opts.keymaps or {})
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

local function clean_review_render(buf, win, items, marked)
  local lines = {}
  local marked_count = 0
  for i = 1, #items do if marked[i] then marked_count = marked_count + 1 end end
  lines[#lines + 1] = ("core.pack: %d of %d marked   <Tab> toggle  a all  u none  <CR> remove  q cancel")
    :format(marked_count, #items)

  local row_to_index = {}
  local highlights = {}
  local row = 2

  -- Name column: longest actual name, floor 16, cap 50.
  local name_max = 16
  for _, p in ipairs(items) do name_max = math.max(name_max, #p.name) end
  if name_max > 50 then name_max = 50 end

  -- Dir column gets remainder. Bytes before dir: 2 (lead) + 3 (glyph) + 2 + name_max + 2
  local PREFIX_BYTES = 2 + 3 + 2 + name_max + 2
  local dir_max
  if win and vim.api.nvim_win_is_valid(win) then
    dir_max = math.max(20, vim.api.nvim_win_get_width(win) - PREFIX_BYTES)
  else
    dir_max = 200
  end

  for i, p in ipairs(items) do
    local glyph = marked[i] and STATUS_MARKED or STATUS_UNMARKED
    local hl_glyph = marked[i] and "Special" or "Comment"
    local name_truncated = p.name
    if #name_truncated > name_max then name_truncated = name_truncated:sub(1, name_max - 1) .. "…" end
    local name = ("%-" .. name_max .. "s"):format(name_truncated)
    local dir = p.dir or ""
    if #dir > dir_max then dir = dir:sub(1, dir_max - 1) .. "…" end
    local line = ("  %s  %s  %s"):format(glyph, name, dir)
    lines[#lines + 1] = line
    row_to_index[row] = i

    local col = 2
    table.insert(highlights, { row - 1, col, col + #glyph, hl_glyph }); col = col + #glyph + 2
    table.insert(highlights, { row - 1, col, col + #name, "Identifier" }); col = col + #name + 2
    table.insert(highlights, { row - 1, col, col + #dir, "Comment" })
    row = row + 1
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.api.nvim_buf_clear_namespace(buf, NS, 0, -1)
  vim.api.nvim_buf_set_extmark(buf, NS, 0, 0, { end_col = #lines[1], hl_group = "Title" })
  for _, hl in ipairs(highlights) do
    vim.api.nvim_buf_set_extmark(buf, NS, hl[1], hl[2], { end_col = hl[3], hl_group = hl[4] })
  end
  return row_to_index
end

function M.clean_review(items, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  if opts.open_window == false then vim.bo[buf].filetype = "PackClean" end
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: clean review")

  local marked = {}
  for i = 1, #items do marked[i] = true end

  local win  -- declared early so render closures can read its width
  local row_to_index = clean_review_render(buf, nil, items, marked)

  if opts.open_window ~= false then
    vim.cmd("topleft 14split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.bo[buf].filetype = "PackClean"  -- after win_set_buf so ftdetect doesn't clear
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
    vim.wo[win].number = false
    vim.wo[win].relativenumber = false
    vim.wo[win].statuscolumn = ""
    vim.wo[win].signcolumn = "no"
    vim.api.nvim_win_set_cursor(win, { 2, 0 })
    vim.cmd("stopinsert")
    vim.api.nvim_create_autocmd("InsertEnter", {
      buffer = buf,
      callback = function() vim.cmd("stopinsert") end,
    })
    -- Re-render now that we have the real window width for dir truncation.
    row_to_index = clean_review_render(buf, win, items, marked)
  end

  local view = { buf = buf, win = win }

  function view:toggle_at(row)
    local i = row_to_index[row]
    if not i then return end
    marked[i] = not marked[i]
    row_to_index = clean_review_render(buf, win, items, marked)
  end

  function view:set_all(value)
    for i = 1, #items do marked[i] = value end
    row_to_index = clean_review_render(buf, win, items, marked)
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
    local km = vim.tbl_extend("force", M.keymaps.clean_review, opts.keymaps or {})
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

local function rollback_review_render(buf, snapshots)
  local lines = {
    ("core.pack: %d snapshots   <CR> restore  q cancel"):format(#snapshots),
    "",
  }
  local highlights = { { 0, 0, #lines[1], "Title" } }
  local row_to_index = {}
  local row = 3

  for i, s in ipairs(snapshots) do
    local idx_str = ("%2d"):format(i)
    local plugins = ("%d plugins"):format(s.plugin_count or 0)
    local line = ("  %s  %s  %s"):format(idx_str, s.iso, plugins)
    lines[#lines + 1] = line
    row_to_index[row] = i

    local col = 2
    table.insert(highlights, { row - 1, col, col + #idx_str, "Number" }); col = col + #idx_str + 2
    table.insert(highlights, { row - 1, col, col + #s.iso, "Identifier" }); col = col + #s.iso + 2
    table.insert(highlights, { row - 1, col, col + #plugins, "Comment" })
    row = row + 1
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.api.nvim_buf_clear_namespace(buf, NS, 0, -1)
  for _, hl in ipairs(highlights) do
    vim.api.nvim_buf_set_extmark(buf, NS, hl[1], hl[2], { end_col = hl[3], hl_group = hl[4] })
  end
  return row_to_index
end

function M.rollback_review(snapshots, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  if opts.open_window == false then vim.bo[buf].filetype = "PackRollback" end
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: rollback")

  local row_to_index = rollback_review_render(buf, snapshots)

  local win
  if opts.open_window ~= false then
    vim.cmd("topleft 14split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.bo[buf].filetype = "PackRollback"  -- after win_set_buf so ftdetect doesn't clear
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
    vim.wo[win].number = false
    vim.wo[win].relativenumber = false
    vim.wo[win].statuscolumn = ""
    vim.wo[win].signcolumn = "no"
    vim.api.nvim_win_set_cursor(win, { 3, 0 })  -- first snapshot row
    vim.cmd("stopinsert")
    vim.api.nvim_create_autocmd("InsertEnter", {
      buffer = buf,
      callback = function() vim.cmd("stopinsert") end,
    })
  end

  local view = { buf = buf, win = win }

  function view:select_at(row)
    local i = row_to_index[row]
    if not i then return end
    if opts.on_select then opts.on_select(snapshots[i]) end
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
    local km = vim.tbl_extend("force", M.keymaps.rollback_review, opts.keymaps or {})
    local function map(lhs, rhs) vim.keymap.set("n", lhs, rhs, { buffer = buf, nowait = true, silent = true }) end
    map(km.select, function() view:select_at(vim.api.nvim_win_get_cursor(win)[1]); view:close() end)
    map(km.cancel, function() view:close() end)
    for _, k in ipairs({ "i", "I", "A", "a", "o", "O", "c", "C", "s", "S", "r", "R", "p", "P" }) do
      vim.keymap.set("n", k, "<nop>", { buffer = buf, nowait = true, silent = true })
    end
  end

  return view
end

-- status(lines, opts) - read-only scratch buffer for displaying status text.
-- opts: { open_window = bool (default true), title = string, highlights = { { row, col_start, col_end, hl_group }, ... } }
function M.status(lines, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  local ft = opts.filetype or "PackStatus"
  if opts.open_window == false then vim.bo[buf].filetype = ft end
  pcall(vim.api.nvim_buf_set_name, buf, opts.title or "core.pack: status")
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false

  if opts.highlights then
    vim.api.nvim_buf_clear_namespace(buf, NS, 0, -1)
    for _, hl in ipairs(opts.highlights) do
      vim.api.nvim_buf_set_extmark(buf, NS, hl[1], hl[2], { end_col = hl[3], hl_group = hl[4] })
    end
  end

  local win
  if opts.open_window ~= false then
    vim.cmd("topleft " .. math.min(math.max(#lines + 2, 8), 24) .. "split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.bo[buf].filetype = ft
    vim.wo[win].wrap = false
    vim.wo[win].cursorline = true
    vim.wo[win].number = false
    vim.wo[win].relativenumber = false
    vim.wo[win].statuscolumn = ""
    vim.wo[win].signcolumn = "no"
    local km = vim.tbl_extend("force", M.keymaps.status, opts.keymaps or {})
    vim.keymap.set("n", km.cancel, "<cmd>close<cr>", { buffer = buf, nowait = true, silent = true })
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

local SPINNER = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
local SPINNER_INTERVAL_MS = 100
local FADE_MS = 1500

function M.fidget(opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  if opts.open_window == false then vim.bo[buf].filetype = "PackFidget" end
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: fidget")

  -- Row state: { name, text, status = "active"|"done"|"error" }, in insertion order.
  local rows = {}
  local row_index = {}
  local spinner_step = 1

  local function render()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    local lines = {}
    for i, r in ipairs(rows) do
      local glyph
      if r.status == "done" then glyph = "✓"
      elseif r.status == "error" then glyph = "✗"
      else glyph = SPINNER[((spinner_step - 1) % #SPINNER) + 1]
      end
      lines[i] = (" %s %s  %s"):format(glyph, r.name, r.text or "")
    end
    if #lines == 0 then lines = { "" } end
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modifiable = false
  end

  local win
  local function open_window()
    if win and vim.api.nvim_win_is_valid(win) then return end
    local width = 0
    for _, r in ipairs(rows) do width = math.max(width, #r.name + #(r.text or "") + 6) end
    width = math.max(width, 30)
    win = vim.api.nvim_open_win(buf, false, {
      relative = "editor", anchor = "SE",
      row = vim.o.lines - 1, col = vim.o.columns,
      width = width, height = math.max(#rows, 1),
      style = "minimal", border = "none",
      focusable = false, zindex = 200,
    })
    vim.bo[buf].filetype = "PackFidget"
  end

  local function resize_window()
    if not win or not vim.api.nvim_win_is_valid(win) then return end
    local width = 0
    for _, r in ipairs(rows) do width = math.max(width, #r.name + #(r.text or "") + 6) end
    width = math.max(width, 30)
    vim.api.nvim_win_set_config(win, {
      relative = "editor", anchor = "SE",
      row = vim.o.lines - 1, col = vim.o.columns,
      width = width, height = math.max(#rows, 1),
    })
  end

  local timer
  local function tick()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    spinner_step = spinner_step + 1
    render()
  end

  local function ensure_timer()
    if opts.open_window == false then return end
    if timer then return end
    timer = vim.uv.new_timer()
    timer:start(SPINNER_INTERVAL_MS, SPINNER_INTERVAL_MS, vim.schedule_wrap(tick))
  end

  local view = { buf = buf, win = win }

  function view:set_status(name, text)
    if row_index[name] then
      rows[row_index[name]].text = text
      rows[row_index[name]].status = "active"
    else
      rows[#rows + 1] = { name = name, text = text, status = "active" }
      row_index[name] = #rows
    end
    render()
    if opts.open_window ~= false then
      open_window()
      resize_window()
      ensure_timer()
    end
  end

  function view:done(name)
    if not row_index[name] then return end
    rows[row_index[name]].status = "done"
    rows[row_index[name]].text = ""
    render()
    -- Schedule fade-out for this row.
    vim.defer_fn(function()
      if not vim.api.nvim_buf_is_valid(buf) then return end
      local idx = row_index[name]
      if not idx then return end
      table.remove(rows, idx)
      row_index[name] = nil
      -- Reindex.
      for i, r in ipairs(rows) do row_index[r.name] = i end
      if #rows == 0 then
        view:close()
      else
        render(); resize_window()
      end
    end, FADE_MS)
  end

  function view:error(name, text)
    if not row_index[name] then
      rows[#rows + 1] = { name = name, text = text, status = "error" }
      row_index[name] = #rows
    else
      rows[row_index[name]].status = "error"
      rows[row_index[name]].text = text
    end
    render()
  end

  function view:close()
    if timer then timer:stop(); timer:close(); timer = nil end
    if win and vim.api.nvim_win_is_valid(win) then
      vim.api.nvim_win_close(win, true)
      win = nil
    end
    if vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end

  return view
end

return M
