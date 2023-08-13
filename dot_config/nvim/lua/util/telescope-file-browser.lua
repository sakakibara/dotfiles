local M = {}

function M.file_browser(opts)
  local fb = require("telescope").extensions.file_browser
  local cwd

  if vim.bo.filetype == "oil" then
    cwd = require("oil").get_current_dir()
  else
    local bufname = vim.api.nvim_buf_get_name(0)
    if bufname == "" then
      cwd = vim.loop.cwd()
    else
      cwd = vim.fn.fnamemodify(bufname, ":p:h")
    end
  end

  local options = vim.tbl_deep_extend("force", {
    path = cwd
  }, opts or {})
  fb.file_browser(options)
end

return M
