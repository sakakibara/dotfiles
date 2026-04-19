-- lua/lib/mason.lua
-- Simple collector for mason-tool-installer's ensure_installed list.
-- Lang files call Lib.mason.add(name1, name2, ...) at module load time;
-- when mason-tool-installer loads on VeryLazy, it reads Lib.mason.list().

local M = {}

local tools = {}
local seen = {}

function M.add(...)
  for _, name in ipairs({ ... }) do
    if type(name) == "string" and not seen[name] then
      seen[name] = true
      tools[#tools + 1] = name
    end
  end
end

function M.list()
  local out = {}
  for i, t in ipairs(tools) do out[i] = t end
  table.sort(out)
  return out
end

function M._reset()
  tools = {}
  seen = {}
end

return M
