---@class util.system
local M = {}

local function is_bitwarden_installed()
  local bitwarden_path = "/Applications/Bitwarden.app"
  local stat = vim.uv.fs_stat(bitwarden_path)
  return stat and stat.type == "directory"
end

function M.role()
  return is_bitwarden_installed() and "work" or "private"
end

return M
