local M = {}

M.home = vim.fn.fnamemodify("~", ":p"):sub(1, -2)

function M.buf_get_name(buf)
  buf = buf or 0
  local name = vim.api.nvim_buf_get_name(buf)
  if vim.bo[buf].filetype == "oil" or name:match("^oil://") then
    local ok, oil = pcall(require, "oil")
    if ok and oil.get_current_dir then return oil.get_current_dir(buf) or "" end
  end
  return name
end

function M.replace_home(p)
  if type(p) ~= "string" or p == "" or M.home == "" then return p end
  if p == M.home then return "~" end
  if p:sub(1, #M.home + 1) == M.home .. "/" then
    return "~" .. p:sub(#M.home + 1)
  end
  return p
end

return M
