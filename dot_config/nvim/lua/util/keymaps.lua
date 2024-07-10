---@class util.keymaps
local M = {}

function M.yank_relative_path()
  vim.fn.setreg("*", vim.fn.fnamemodify(Util.path.get_current_file_path(), ":~:."))
end

function M.yank_full_path()
  vim.fn.setreg("*", Util.path.get_current_file_path())
end

function M.feed_escape()
  local esc = vim.api.nvim_replace_termcodes("<Esc>", true, true, true)
  vim.api.nvim_feedkeys(esc, "x", false)
end

function M.create_undo()
  local create_undo = vim.api.nvim_replace_termcodes("<C-g>u", true, true, true)
  if vim.api.nvim_get_mode().mode == "i" then
    vim.api.nvim_feedkeys(create_undo, "n", false)
  end
end

return M
