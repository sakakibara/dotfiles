---@class util.system
local M = {}

local function is_skysea_installed()
  local skysea_path = "/Applications/SKYSEAClientView.app"
  local stat = vim.uv.fs_stat(skysea_path)
  return stat and stat.type == "directory"
end

function M.role()
  return is_skysea_installed() and "work" or "private"
end

return M
