local upath = require("util.path")

local M = {}

local KeymapsUtil = {}

function KeymapsUtil.put_empty_line(put_above)
  if type(put_above) == "boolean" then
    vim.o.operatorfunc = "v:lua.KeymapsUtil.put_empty_line"
    KeymapsUtil.cache_empty_line = { put_above = put_above }
    return "g@l"
  end

  local target_line = vim.fn.line(".") - (KeymapsUtil.cache_empty_line.put_above and 1 or 0)
  ---@diagnostic disable-next-line: param-type-mismatch
  vim.fn.append(target_line, vim.fn["repeat"]({ "" }, vim.v.count1))
end

function M.setup()
  _G.KeymapsUtil = KeymapsUtil
end

function M.yank_relative_path()
  vim.fn.setreg("*", vim.fn.fnamemodify(upath.get_current_file_path(), ":~:."))
end

function M.yank_full_path()
  vim.fn.setreg("*", upath.get_current_file_path())
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
