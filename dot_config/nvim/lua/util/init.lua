local LazyUtil = require("lazy.core.util")

---@class util: LazyUtilCore
---@field config ConfigOptions
---@field buffer util.buffer
---@field cmp util.cmp
---@field file util.file
---@field format util.format
---@field keymaps util.keymaps
---@field lsp util.lsp
---@field notes util.notes
---@field path util.path
---@field plugin util.plugin
---@field root util.root
---@field telescope util.telescope
---@field terminal util.terminal
---@field toggle util.toggle
local M = {}

setmetatable(M, {
  __index = function(t, k)
    if LazyUtil[k] then
      return LazyUtil[k]
    end
    ---@diagnostic disable-next-line: no-unknown
    t[k] = require("util." .. k)
    return t[k]
  end,
})

return M
