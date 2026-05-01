local C = require("core.pack.ui._common")
local M = {}

local function render(buf, snapshots)
  local lines = {
    ("core.pack: %d snapshots   <CR> restore  q cancel"):format(#snapshots),
    "",
  }
  local highlights = { { 0, 0, #lines[1], "Title" } }
  local row_to_index = {}
  local row = 3

  for i, s in ipairs(snapshots) do
    local idx_str = ("%2d"):format(i)
    local ago_str = ("%-9s"):format(C.format_ago(s.ts or 0))
    local plugins = ("%d plugins"):format(s.plugin_count or 0)
    local line = ("  %s  %s  %s  %s"):format(idx_str, s.iso, ago_str, plugins)
    lines[#lines + 1] = line
    row_to_index[row] = i

    local col = 2
    table.insert(highlights, { row - 1, col, col + #idx_str, "Number" }); col = col + #idx_str + 2
    table.insert(highlights, { row - 1, col, col + #s.iso, "Identifier" }); col = col + #s.iso + 2
    table.insert(highlights, { row - 1, col, col + #ago_str, "DiagnosticHint" }); col = col + #ago_str + 2
    table.insert(highlights, { row - 1, col, col + #plugins, "Comment" })
    row = row + 1
  end

  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.api.nvim_buf_clear_namespace(buf, C.NS, 0, -1)
  for _, hl in ipairs(highlights) do
    vim.api.nvim_buf_set_extmark(buf, C.NS, hl[1], hl[2], { end_col = hl[3], hl_group = hl[4] })
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

  local row_to_index = render(buf, snapshots)

  local win
  if opts.open_window ~= false then
    vim.cmd("topleft 14split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.bo[buf].filetype = "PackRollback"  -- after win_set_buf so ftdetect doesn't clear
    C.lock_pack_window(buf, win)
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
    local km = vim.tbl_extend("force", C.keymaps.rollback_review, opts.keymaps or {})
    local function map(lhs, rhs) vim.keymap.set("n", lhs, rhs, { buffer = buf, nowait = true, silent = true }) end
    map(km.select, function() view:select_at(vim.api.nvim_win_get_cursor(win)[1]); view:close() end)
    map(km.cancel, function() view:close() end)
    for _, k in ipairs({ "i", "I", "A", "a", "o", "O", "c", "C", "s", "S", "r", "R", "p", "P" }) do
      vim.keymap.set("n", k, "<nop>", { buffer = buf, nowait = true, silent = true })
    end
  end

  return view
end

return M
