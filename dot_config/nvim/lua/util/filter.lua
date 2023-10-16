local kinds = require("config.kinds")
local M = {}

function M.get_kind_filter(buf)
  buf = (buf == nil or buf == 0) and vim.api.nvim_get_current_buf() or buf
  local ft = vim.bo[buf].filetype
  if kinds == false then
    return
  end
  if kinds[ft] == false then
    return
  end
  ---@diagnostic disable-next-line: return-type-mismatch
  return type(kinds) == "table" and type(kinds.default) == "table" and kinds.default or nil
end

return M
