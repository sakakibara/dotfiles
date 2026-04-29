-- lua/lib/keymaps.lua
local M = {}

function M.yank_relative_path()
  local name = vim.api.nvim_buf_get_name(0)
  local rel = vim.fn.fnamemodify(name, ":.")
  vim.fn.setreg("+", rel)
  vim.notify("`" .. rel .. "`", vim.log.levels.INFO, { title = "Yanked" })
end

function M.yank_full_path()
  local name = vim.api.nvim_buf_get_name(0)
  local abs = vim.fn.fnamemodify(name, ":p")
  vim.fn.setreg("+", abs)
  vim.notify("`" .. abs .. "`", vim.log.levels.INFO, { title = "Yanked" })
end

function M.put_empty_line(above)
  local count = vim.v.count1
  local target = vim.api.nvim_win_get_cursor(0)[1] + (above and -1 or 0)
  local blanks = {}
  for _ = 1, count do blanks[#blanks + 1] = "" end
  vim.api.nvim_buf_set_lines(0, target, target, true, blanks)
end

-- Filetype picker. Used by the statusline filetype segment's click handler
-- and by the <Leader>ut keymap. Snacks.picker.select gives fuzzy filter over
-- the ~200-entry filetype list; falls back to vim.ui.select if snacks isn't up.
function M.pick_filetype()
  local items = vim.fn.getcompletion("", "filetype")
  table.sort(items)
  local select = (Snacks and Snacks.picker and Snacks.picker.select) or vim.ui.select
  select(items, { prompt = "Set filetype: " }, function(choice)
    if choice and choice ~= "" then vim.bo.filetype = choice end
  end)
end

return M
