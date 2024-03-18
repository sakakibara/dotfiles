local ubuffer = require("util.buffer")
local upath = require("util.path")
local M = {}

M.notes_root = upath.home .. upath.sep .. "Notes"
M.journal_root = M.notes_root .. upath.sep .. "journal"

function M.open_journal(time)
  if time == nil then
    time = os.time()
  end
  local date = os.date("*t", time)
  local path = M.journal_root
    .. upath.sep
    .. string.format("%04d", date.year)
    .. upath.sep
    .. string.format("%02d", date.month)
    .. upath.sep
    .. string.format("%02d", date.day)
    .. ".md"
  vim.cmd("edit " .. path)
  local buf = vim.api.nvim_get_current_buf()
  if ubuffer.is_empty(buf) then
    vim.api.nvim_buf_set_lines(buf, 0, 0, false, { "# " .. os.date("%Y-%m-%d", time) })
  end
end

return M
