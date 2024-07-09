local LazyUtil = require("lazy.core.util")

---@class util: LazyUtilCore
---@field config ConfigOptions
---@field buffer util.buffer
---@field cmp util.cmp
---@field extras util.extras
---@field file util.file
---@field format util.format
---@field json util.json
---@field keymaps util.keymaps
---@field lsp util.lsp
---@field notes util.notes
---@field path util.path
---@field pick util.pick
---@field plugin util.plugin
---@field root util.root
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

local cache = {}

function M.memoize(fn)
  return function(...)
    local key = vim.inspect({ ... })
    if cache[key] == nil then
      cache[key] = fn(...)
    end
    return cache[key]
  end
end

return M
