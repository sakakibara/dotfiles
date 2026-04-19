-- lua/lib/neotest.lua
-- Registry for neotest adapters. Lang files call Lib.neotest.add(name, factory);
-- when neotest's config runs on VeryLazy, it reads Lib.neotest.list() to populate
-- its `adapters` option.

local M = {}

local factories = {}
local seen = {}

function M.add(name, factory)
  if type(name) ~= "string" or seen[name] then return end
  if type(factory) ~= "function" then return end
  seen[name] = true
  factories[#factories + 1] = { name = name, factory = factory }
end

function M.list()
  local out = {}
  for _, f in ipairs(factories) do
    local ok, adapter = pcall(f.factory)
    if ok and adapter ~= nil then
      out[#out + 1] = adapter
    elseif not ok then
      vim.notify(("Lib.neotest: adapter %s failed to load: %s"):format(f.name, adapter), vim.log.levels.WARN)
    end
  end
  return out
end

function M._reset()
  factories = {}
  seen = {}
end

return M
