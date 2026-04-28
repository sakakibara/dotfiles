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
  vim.api.nvim_buf_set_name(buf, opts.title or "core.pack: install")

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

return M
