local C = require("core.pack.ui._common")
local M = {}

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
    vim.api.nvim_buf_clear_namespace(buf, C.NS, 0, -1)
    for _, hl in ipairs(opts.highlights) do
      vim.api.nvim_buf_set_extmark(buf, C.NS, hl[1], hl[2], { end_col = hl[3], hl_group = hl[4] })
    end
  end

  local win
  if opts.open_window ~= false then
    vim.cmd("topleft " .. math.min(math.max(#lines + 2, 8), 24) .. "split")
    win = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(win, buf)
    vim.bo[buf].filetype = ft
    C.lock_pack_window(buf, win)
    local km = vim.tbl_extend("force", C.keymaps.status, opts.keymaps or {})
    vim.keymap.set("n", km.cancel, "<cmd>close<cr>", { buffer = buf, nowait = true, silent = true })
    if opts.on_filter then
      vim.keymap.set("n", "f", function()
        vim.ui.input({ prompt = "filter: " }, function(input)
          opts.on_filter(input or "")
        end)
      end, { buffer = buf, nowait = true, silent = true })
      vim.keymap.set("n", "F", function() opts.on_filter("") end,
        { buffer = buf, nowait = true, silent = true })
    end
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
