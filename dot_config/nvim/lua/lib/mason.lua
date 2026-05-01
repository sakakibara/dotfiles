-- lua/lib/mason.lua
-- Tool registry for mason. Tools can be either:
--   eager      — install on startup regardless of context
--                (Lib.mason.add(name))
--   on-demand  — install only when a buffer of `ft` first opens
--                (Lib.mason.add(name, { ft = "<ft>" }))
--
-- The on-demand tools are typically LSP servers / formatters / linters
-- tied to a specific filetype. Eager tools are filetype-agnostic
-- (build tools, generic helpers).

local M = {}

local eager = {}      -- list of names
local eager_seen = {} -- name → true
local by_ft = {}      -- ft → list of names
local ft_seen = {}    -- (ft .. ":" .. name) → true

local function add_eager(name)
  if eager_seen[name] then return end
  eager_seen[name] = true
  eager[#eager + 1] = name
end

local function add_for_ft(name, ft)
  by_ft[ft] = by_ft[ft] or {}
  local key = ft .. ":" .. name
  if ft_seen[key] then return end
  ft_seen[key] = true
  table.insert(by_ft[ft], name)
end

-- Lib.mason.add(name)                          -- eager
-- Lib.mason.add(name, { ft = "lua" })          -- on-demand single ft
-- Lib.mason.add(name, { ft = { "ts", "tsx" }}) -- on-demand multi ft
-- Lib.mason.add(name1, name2, ...)             -- eager (variadic, back-compat)
function M.add(...)
  local args = { ... }
  local last = args[#args]
  local opts = type(last) == "table" and last or nil
  if opts then args[#args] = nil end

  for _, name in ipairs(args) do
    if type(name) ~= "string" then goto continue end
    if opts and opts.ft then
      local fts = type(opts.ft) == "table" and opts.ft or { opts.ft }
      for _, ft in ipairs(fts) do
        add_for_ft(name, ft)
      end
    else
      add_eager(name)
    end
    ::continue::
  end
end

-- All eager tools (sorted, deduped). Returned as a list.
function M.eager_list()
  local out = {}
  for i, t in ipairs(eager) do out[i] = t end
  table.sort(out)
  return out
end

-- Tools tagged for `ft` (sorted). Empty list if none.
function M.list_for_ft(ft)
  local out = {}
  for _, t in ipairs(by_ft[ft] or {}) do out[#out + 1] = t end
  table.sort(out)
  return out
end

-- All registered tools (eager + every ft), sorted, deduped. Used by
-- diagnostic / health checks.
function M.list()
  local seen = {}
  local out = {}
  for _, t in ipairs(eager) do
    if not seen[t] then seen[t] = true; out[#out + 1] = t end
  end
  for _, names in pairs(by_ft) do
    for _, t in ipairs(names) do
      if not seen[t] then seen[t] = true; out[#out + 1] = t end
    end
  end
  table.sort(out)
  return out
end

function M._reset()
  eager, eager_seen, by_ft, ft_seen = {}, {}, {}, {}
end

return M
