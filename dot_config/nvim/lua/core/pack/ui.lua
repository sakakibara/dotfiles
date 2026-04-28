local M = {}

local STATUS_GLYPH = { pending = "[ ]", ok = "[✓]", error = "[✗]", working = "[…]" }

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

local function format_review_line(p, marked)
  return ("  [%s] %-30s  %s..%s  (%d commits)"):format(
    marked and "x" or " ", p.name, p.from:sub(1, 7), p.to:sub(1, 7), p.count or 0)
end

-- update_review(pending, opts)
-- pending: { { name, from, to, count, dir, ... }, ... }
-- opts: { open_window = bool, on_apply = function(marked_entries) end }
function M.update_review(pending, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: update review")

  local marked = {}  -- index 1..#pending → bool
  for i = 1, #pending do marked[i] = true end

  local function header()
    local count = 0
    for i = 1, #pending do if marked[i] then count = count + 1 end end
    return ("%d/%d marked. <Tab> toggle, a all, u none, <CR> apply, q cancel."):format(count, #pending)
  end

  local function render()
    local lines = { header() }
    for i, p in ipairs(pending) do
      lines[#lines + 1] = format_review_line(p, marked[i])
    end
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modifiable = false
  end
  render()

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
    local i = row - 1
    if i < 1 or i > #pending then return end
    marked[i] = not marked[i]
    render()
  end

  function view:set_all(value)
    for i = 1, #pending do marked[i] = value end
    render()
  end

  function view:apply()
    local out = {}
    for i, p in ipairs(pending) do if marked[i] then out[#out + 1] = p end end
    if opts.on_apply then opts.on_apply(out) end
  end

  function view:close()
    if self.win and vim.api.nvim_win_is_valid(self.win) then
      vim.api.nvim_win_close(self.win, true)
    end
    if vim.api.nvim_buf_is_valid(self.buf) then
      vim.api.nvim_buf_delete(self.buf, { force = true })
    end
  end

  -- Buffer-local maps: only install when a window exists.
  if win then
    local function map(lhs, rhs) vim.keymap.set("n", lhs, rhs, { buffer = buf, nowait = true, silent = true }) end
    map("<Tab>", function() view:toggle_at(vim.api.nvim_win_get_cursor(win)[1]) end)
    map("x",     function() view:toggle_at(vim.api.nvim_win_get_cursor(win)[1]) end)
    map("a",     function() view:set_all(true) end)
    map("u",     function() view:set_all(false) end)
    map("<CR>",  function() view:apply(); view:close() end)
    map("q",     function() view:close() end)
  end

  return view
end

return M
