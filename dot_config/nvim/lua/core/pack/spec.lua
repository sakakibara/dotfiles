-- lua/core/pack/spec.lua
local M = {}

local notify = require("core.pack.util").notify

local ALLOWED_FIELDS = {
  [1] = true, src = true, name = true, version = true, branch = true,
  dependencies = true, event = true, ft = true, cmd = true, keys = true,
  priority = true, enabled = true, cond = true, dev = true, lazy = true,
  init = true, opts = true, config = true, main = true, build = true,
}

function M.normalize(spec)
  if type(spec) == "string" then spec = { spec } end
  if type(spec) ~= "table" then error("spec must be a table, got " .. type(spec)) end

  local src, name = spec.src, spec.name
  if spec[1] and not src then
    local shorthand = spec[1]
    if shorthand:match("^https?://") then
      src = shorthand
    else
      src = "https://github.com/" .. shorthand
    end
    if not name then name = shorthand:match("([^/]+)$") end
  end
  if not src and not spec.dev then error("spec missing src: " .. vim.inspect(spec)) end
  if not name then error("spec missing name (could not infer): " .. vim.inspect(spec)) end

  for k in pairs(spec) do
    if type(k) == "string" and not ALLOWED_FIELDS[k] then
      notify(("core.pack: unknown field '%s' on spec '%s'"):format(k, name), vim.log.levels.WARN)
    end
  end

  local has_trigger = spec.event or spec.ft or spec.cmd or spec.keys
  local lazy = spec.lazy
  if lazy == nil then lazy = has_trigger ~= nil end

  local version = spec.version
  if version == nil and type(spec.branch) == "string" then
    version = spec.branch
  end
  if type(version) == "string" and version:match("[%*x%^~]") then
    local stripped = version:gsub("[%*x%^~]", ""):gsub("%.%.+", "."):gsub("^%.", ""):gsub("%.$", "")
    version = vim.version.range(stripped)
  end

  return {
    src = src,
    name = name,
    version = version,
    branch = spec.branch,
    dependencies = spec.dependencies or {},
    event = spec.event,
    ft = spec.ft,
    cmd = spec.cmd,
    keys = spec.keys,
    priority = spec.priority or 50,
    enabled = spec.enabled ~= false,
    cond = spec.cond,
    dev = spec.dev == true,
    lazy = lazy,
    init = spec.init,
    opts = spec.opts or {},
    config = spec.config,
    main = spec.main,
    build = spec.build,
  }
end

-- Union two list-like tables, dedup'd by string equality (or table identity).
local function union(a, b)
  if type(a) ~= "table" then a = a == nil and {} or { a } end
  if type(b) ~= "table" then b = b == nil and {} or { b } end
  local seen = {}
  local out = {}
  local function add(v)
    if seen[v] then return end
    seen[v] = true
    out[#out + 1] = v
  end
  for _, v in ipairs(a) do add(v) end
  for _, v in ipairs(b) do add(v) end
  return out
end

-- Merge two canonical specs, b shadowing a. Returns (merged, warnings).
-- Collections (deps/keys/event/ft/cmd/opts) union or deep-merge.
-- Booleans use most-eager-wins (any non-lazy → eager, any disabled →
-- disabled, any dev → dev). Scalars (src/version/branch/build/init/
-- config/main/cond) use last-non-nil-wins; conflicts on different
-- non-nil values produce a warning naming the field.
function M._merge(a, b)
  local merged = vim.deepcopy(a)
  -- Collections
  merged.dependencies = union(a.dependencies, b.dependencies)
  merged.keys         = union(a.keys, b.keys)
  merged.event        = union(a.event, b.event)
  merged.ft           = union(a.ft, b.ft)
  merged.cmd          = union(a.cmd, b.cmd)
  merged.opts         = vim.tbl_deep_extend("force", a.opts or {}, b.opts or {})
  -- Most-eager-wins booleans
  merged.priority = math.max(a.priority or 50, b.priority or 50)
  merged.lazy     = a.lazy and b.lazy
  merged.enabled  = a.enabled and b.enabled
  merged.dev      = a.dev or b.dev
  -- Scalar last-non-nil-wins with conflict detection.
  -- vim.deep_equal handles every type correctly:
  --   - same string / number / boolean        -> equal -> silent
  --   - same function reference               -> equal -> silent
  --   - different functions (any identities)  -> not equal -> warn
  --   - different tables (deep)                -> compared deeply
  local warnings = {}
  local CONFLICT_FIELDS = {
    "src", "version", "branch", "build", "init", "config", "main", "cond",
  }
  local function fmt_value(v)
    if type(v) == "function" then return "<function>" end
    if type(v) == "table" then return vim.inspect(v):gsub("\n", " "):sub(1, 60) end
    return tostring(v)
  end
  for _, k in ipairs(CONFLICT_FIELDS) do
    -- Skip version conflict when version was derived from branch (same string);
    -- the branch field will emit its own conflict warning for that case.
    local derived_from_branch = k == "version"
      and type(a[k]) == "string" and a[k] == a.branch
      and type(b[k]) == "string" and b[k] == b.branch
    if not derived_from_branch
      and a[k] ~= nil and b[k] ~= nil and not vim.deep_equal(a[k], b[k])
    then
      table.insert(warnings, ("%s: conflicting %s: %s vs %s; using %s")
        :format(merged.name, k, fmt_value(a[k]), fmt_value(b[k]), fmt_value(b[k])))
    end
    if b[k] ~= nil then merged[k] = b[k] end
  end
  return merged, warnings
end

-- Resolve a list of raw spec entries into a deduped, merged list of
-- canonical specs. First occurrence of each name determines its position;
-- later occurrences merge into the first via _merge.
function M.resolve(raw_specs)
  local by_name = {}
  local order = {}
  local warnings = {}
  for _, raw in ipairs(raw_specs) do
    local s = M.normalize(raw)
    if by_name[s.name] then
      local merged, ws = M._merge(by_name[s.name], s)
      by_name[s.name] = merged
      for _, w in ipairs(ws) do warnings[#warnings + 1] = w end
    else
      by_name[s.name] = s
      order[#order + 1] = s.name
    end
  end
  local specs = {}
  for _, name in ipairs(order) do specs[#specs + 1] = by_name[name] end
  return specs, warnings
end

return M
