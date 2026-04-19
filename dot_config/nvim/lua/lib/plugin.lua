-- lua/lib/plugin.lua
local M = {}

function M.has(name)   return require("core.pack").has(name) end
function M.loaded(name) return require("core.pack").loaded(name) end
function M.opts(name)  return require("core.pack").opts(name) end
function M.on_load(name, fn) require("core.pack").on_load(name, fn) end

return M
