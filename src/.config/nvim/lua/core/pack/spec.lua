local M = {}


local ALLOWED_FIELDS = {
  [1] = true, src = true, name = true, version = true, branch = true,
  dependencies = true, event = true, ft = true, cmd = true, keys = true,
  priority = true, enabled = true, cond = true, dev = true, lazy = true,
  init = true, opts = true, config = true, main = true, build = true,
  npm = true, files = true, min_age = true,
}

local function is_range(s)
  s = vim.trim(s)
  if s == "*" or s:match("^[%^~<>=]") or s:match("%s%-%s") then return true end
  local wildcard = false
  for _, seg in ipairs(vim.split((s:gsub("^[vV]", "")), ".", { plain = true })) do
    if seg:match("^[xX%*]$") then
      wildcard = true
    elseif not seg:match("^%d+$") then
      return false
    end
  end
  return wildcard
end

function M.normalize(spec)
  if type(spec) == "string" then spec = { spec } end
  if type(spec) ~= "table" then error("spec must be a table, got " .. type(spec)) end

  local src, name = spec.src, spec.name
  if spec.npm ~= nil then
    local Npm = require("core.pack.npm")
    if not Npm.valid_name(spec.npm) then error("invalid npm package name: " .. vim.inspect(spec.npm)) end
    if type(spec.files) ~= "table" or #spec.files == 0 then
      error("npm spec needs a non-empty files list: " .. spec.npm)
    end
    for _, f in ipairs(spec.files) do
      if not Npm.valid_file(f) then error(("npm spec %s: invalid file path %s"):format(spec.npm, vim.inspect(f))) end
    end
    if spec.min_age ~= nil and (type(spec.min_age) ~= "number" or spec.min_age < 0) then
      error("npm spec min_age must be a non-negative number of days: " .. spec.npm)
    end
    src = src or ("https://www.npmjs.com/package/" .. spec.npm)
    name = name or Npm.default_name(spec.npm)
  elseif spec[1] and not src then
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
      vim.notify(("core.pack: unknown field '%s' on spec '%s'"):format(k, name), vim.log.levels.WARN)
    end
  end

  local has_trigger = spec.event or spec.ft or spec.cmd or spec.keys
  local lazy = spec.lazy
  if lazy == nil then lazy = has_trigger ~= nil end
  if spec.npm ~= nil then lazy = true end

  local version = spec.version
  if type(version) == "string" and is_range(version) then
    version = vim.version.range(version) or version
  end
  if version == nil and type(spec.branch) == "string" then
    version = spec.branch
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
    npm = spec.npm,
    npm_version = spec.npm ~= nil and spec.version or nil,
    files = spec.files,
    min_age = spec.min_age,
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
-- Booleans use most-eager-wins (any non-lazy -> eager, any disabled ->
-- disabled, any dev -> dev). Scalars (src/version/branch/build/init/
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
    "npm", "npm_version", "files", "min_age",
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
