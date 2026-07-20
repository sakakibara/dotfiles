-- Tool registry for mason. All tools are tied to one or more filetypes
-- and install on-demand the first time a buffer of that filetype opens.
-- (No eager / unconditional install path: every tool we know about
-- belongs to a language, and we only want to install for languages the
-- user actually edits.)

local M = {}

local by_ft = {}      -- ft → list of names
local seen  = {}      -- (ft .. ":" .. name) → true

local function add_for_ft(name, ft)
  by_ft[ft] = by_ft[ft] or {}
  local key = ft .. ":" .. name
  if seen[key] then return end
  seen[key] = true
  table.insert(by_ft[ft], name)
end

-- Lib.mason.add(name1, name2, ..., { ft = "lua" })
-- Lib.mason.add(name, { ft = { "ts", "tsx" } })
-- The trailing opts table with `ft` is required.
function M.add(...)
  local args = { ... }
  local last = args[#args]
  local opts = type(last) == "table" and last or nil
  if not opts or not opts.ft then
    error("Lib.mason.add: trailing opts table with ft is required")
  end
  args[#args] = nil

  local fts = type(opts.ft) == "table" and opts.ft or { opts.ft }
  for _, name in ipairs(args) do
    if type(name) == "string" then
      for _, ft in ipairs(fts) do add_for_ft(name, ft) end
    end
  end
end

-- Tools tagged for `ft` (sorted). Empty list if none.
function M.list_for_ft(ft)
  local out = {}
  for _, t in ipairs(by_ft[ft] or {}) do out[#out + 1] = t end
  table.sort(out)
  return out
end

-- All registered tools across every ft, sorted, deduped. For diagnostics.
function M.list()
  local seen_name = {}
  local out = {}
  for _, names in pairs(by_ft) do
    for _, t in ipairs(names) do
      if not seen_name[t] then seen_name[t] = true; out[#out + 1] = t end
    end
  end
  table.sort(out)
  return out
end

function M._reset()
  by_ft, seen = {}, {}
end

return M
