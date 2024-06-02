---@class util.notes
local M = {}

M.notes_root = Util.path.home .. Util.path.sep .. "Notes"
M.journal_root = M.notes_root .. Util.path.sep .. "journal"

function M.open_journal(time)
  if time == nil then
    time = os.time()
  end
  local date = os.date("*t", time)
  local path = M.journal_root
    .. Util.path.sep
    .. string.format("%04d", date.year)
    .. Util.path.sep
    .. string.format("%02d", date.month)
    .. Util.path.sep
    .. string.format("%02d", date.day)
    .. ".md"
  vim.cmd("edit " .. path)
  local buf = vim.api.nvim_get_current_buf()
  if Util.buffer.is_empty(buf) then
    vim.api.nvim_buf_set_lines(buf, 0, 0, false, { "# " .. os.date("%Y-%m-%d", time) })
  end
end

return M
