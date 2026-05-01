-- lua/lib/parsers.lua
-- Registry of treesitter parsers, keyed by filetype. Lang specs and
-- direct callers register parsers via Lib.parsers.add(name, { ft = ... });
-- the treesitter spec's FileType autocmd reads list_for_ft(ft) to
-- decide which parsers to install on first open of that filetype.

local M = {}

local by_ft = {} -- ft → list of parser names
local seen  = {} -- (ft .. ":" .. name) → true

local function add_for_ft(name, ft)
  by_ft[ft] = by_ft[ft] or {}
  local key = ft .. ":" .. name
  if seen[key] then return end
  seen[key] = true
  table.insert(by_ft[ft], name)
end

-- Lib.parsers.add(name1, name2, ..., { ft = "ts" })
-- Lib.parsers.add(name, { ft = { "tsx", "typescript" } })
-- Trailing opts table with `ft` is required.
function M.add(...)
  local args = { ... }
  local last = args[#args]
  local opts = type(last) == "table" and last or nil
  if not opts or not opts.ft then
    error("Lib.parsers.add: trailing opts table with ft is required")
  end
  args[#args] = nil

  local fts = type(opts.ft) == "table" and opts.ft or { opts.ft }
  for _, name in ipairs(args) do
    if type(name) == "string" then
      for _, ft in ipairs(fts) do add_for_ft(name, ft) end
    end
  end
end

function M.list_for_ft(ft)
  local out = {}
  for _, t in ipairs(by_ft[ft] or {}) do out[#out + 1] = t end
  table.sort(out)
  return out
end

-- All registered fts. Used by treesitter's FileType autocmd to know
-- which fts have a registered parser (and thus should get treesitter
-- highlighting).
function M.fts()
  local out = {}
  for ft in pairs(by_ft) do out[#out + 1] = ft end
  table.sort(out)
  return out
end

function M._reset()
  by_ft, seen = {}, {}
end

return M
