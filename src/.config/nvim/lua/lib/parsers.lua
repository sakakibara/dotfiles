-- Registry of treesitter parsers, keyed by filetype. Lang specs and
-- direct callers register parsers via Lib.parsers.add(name, { ft = ... });
-- the treesitter spec's FileType autocmd reads list_for_ft(ft) to
-- decide which parsers to install on first open of that filetype.

local M = {}

local by_ft = {} -- ft → list of parser names
local seen  = {} -- (ft .. ":" .. name) → true
local eager = {} -- name → true (parsers to install at startup, ft-independent)

local function add_for_ft(name, ft)
  by_ft[ft] = by_ft[ft] or {}
  local key = ft .. ":" .. name
  if seen[key] then return end
  seen[key] = true
  table.insert(by_ft[ft], name)
end

-- Lib.parsers.add(name1, name2, ..., { ft = "ts" })
-- Lib.parsers.add(name, { ft = { "tsx", "typescript" } })
-- Lib.parsers.add(name, { ft = "regex", eager = true })
-- `eager = true` also installs the parser at startup regardless of ft —
-- needed for parsers that serve cross-cutting consumers (noice cmdline
-- regex highlighting, snacks.picker) rather than a buffer of that ft.
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
      if opts.eager then eager[name] = true end
    end
  end
end

function M.eager_list()
  local out = {}
  for name in pairs(eager) do out[#out + 1] = name end
  table.sort(out)
  return out
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

-- Parsers wanted by a buffer's org #+begin_src blocks, via organ.nvim's
-- public helper (org-babel spellings already resolved to parser names).
-- Feature-detected: returns {} when organ is absent or predates the
-- helper, so callers treat "no injected languages" uniformly.
function M.src_block_for_buf(bufnr)
  local ok, organ = pcall(require, "organ")
  if not ok or type(organ.src_block_parsers) ~= "function" then return {} end
  local got, list = pcall(organ.src_block_parsers, bufnr)
  if not got or type(list) ~= "table" then return {} end
  return list
end

function M._reset()
  by_ft, seen, eager = {}, {}, {}
end

return M
