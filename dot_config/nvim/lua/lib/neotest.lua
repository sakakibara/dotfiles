-- Registry for neotest adapters. Lang files call Lib.neotest.add(name, factory);
-- neotest's spec calls Lib.neotest.setup(opts) which stashes opts and applies
-- them with the current adapter list.
--
-- Ordering problem this solves: lang files register adapters via Lib.neotest.add
-- at plugin-spec-registration time, but their host plugins (e.g. neotest-go)
-- are ft-lazy with `dependencies = { "neotest" }`. When a Go file ft-triggers
-- neotest-go, core.pack's load_spec walks deps first — neotest loads fully
-- (packadd + config) BEFORE neotest-go packadds. So at the moment
-- require("neotest").setup runs, require("neotest-go") still fails.
--
-- Fix: list() skips adapters whose host plugin isn't yet loaded (not a
-- warning — it's the expected state during dep load). When an adapter's host
-- plugin eventually loads, an on_load hook re-applies neotest.setup with the
-- now-expanded adapter list. The on_load pattern is idempotent: neotest.setup
-- replaces adapters wholesale, and late-add is the documented upstream path
-- (lazy.nvim configs do the same).

local M = {}

local factories = {}
local seen = {}
local stashed_opts

local function resolve(f)
  if not Lib.plugin.loaded(f.name) then return nil end
  local ok, adapter = pcall(f.factory)
  if not ok then
    vim.notify(("Lib.neotest: %s factory errored: %s"):format(f.name, adapter), vim.log.levels.WARN)
    return nil
  end
  return adapter
end

local function apply()
  if not (stashed_opts and Lib.plugin.loaded("neotest")) then return end
  local opts = vim.deepcopy(stashed_opts)
  opts.adapters = M.list()
  require("neotest").setup(opts)
end

function M.add(name, factory)
  if type(name) ~= "string" or seen[name] then return end
  if type(factory) ~= "function" then return end
  seen[name] = true
  table.insert(factories, { name = name, factory = factory })
  -- Re-apply when the host plugin loads, so adapters that packadd after
  -- neotest's initial setup (the common case — see header) still land.
  Lib.plugin.on_load(name, apply)
end

function M.list()
  local out = {}
  for _, f in ipairs(factories) do
    local adapter = resolve(f)
    if adapter ~= nil then out[#out + 1] = adapter end
  end
  return out
end

function M.setup(opts)
  stashed_opts = opts
  apply()
end

function M._reset()
  factories = {}
  seen = {}
  stashed_opts = nil
end

return M
