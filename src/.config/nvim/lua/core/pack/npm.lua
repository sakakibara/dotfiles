local M = {}

M.REGISTRY = "https://registry.npmjs.org"
M.MARKER = ".core-pack-npm.json"
M.DEFAULT_MIN_AGE = 7
M.min_age = nil
M._request = nil
M._now = nil

local DAY = 86400

local function now()
  return M._now and M._now() or os.time()
end

local function request(url, opts, cb)
  if M._request then return M._request(url, opts, cb) end
  vim.net.request(url, opts, function(err, res)
    vim.schedule(function() cb(err, res and res.body) end)
  end)
end

local function encode_name(pkg)
  return (pkg:gsub("/", "%%2f"))
end

function M.valid_name(pkg)
  return type(pkg) == "string"
    and (pkg:match("^@[%w][%w%-%._~]*/[%w][%w%-%._~]*$") or pkg:match("^[%w][%w%-%._~]*$")) ~= nil
end

function M.default_name(pkg)
  return (pkg:gsub("^@", ""):gsub("/", "-"))
end

function M.min_age_days(spec)
  local d = spec and spec.min_age
  if d == nil then d = M.min_age end
  if d == nil then d = M.DEFAULT_MIN_AGE end
  return d
end

local function iso_to_time(s)
  if type(s) ~= "string" then return nil end
  local y, mo, d, h, mi, sec = s:match("^(%d+)%-(%d+)%-(%d+)T(%d+):(%d+):(%d+)")
  if not y then return nil end
  local t = os.time({ year = tonumber(y), month = tonumber(mo), day = tonumber(d),
    hour = tonumber(h), min = tonumber(mi), sec = tonumber(sec), isdst = false })
  local utc = os.date("!*t", t)
  local loc = os.date("*t", t)
  utc.isdst = loc.isdst
  return t + (t - os.time(utc))
end

function M.format_age(seconds)
  if seconds < DAY then return ("%dh"):format(math.floor(seconds / 3600)) end
  return ("%dd"):format(math.floor(seconds / DAY))
end

local function range_of(spec)
  local v = spec.version
  if type(v) == "table" then return v end
  if type(v) == "string" then return vim.version.range(v) end
  return vim.version.range("*")
end

function M.usable_lock(spec, entry)
  if type(entry) ~= "table" or entry.npm ~= spec.npm or type(entry.version) ~= "string"
    or type(entry.tarball) ~= "string" or type(entry.integrity) ~= "string" then
    return false
  end
  local range = range_of(spec)
  local parsed = vim.version.parse(entry.version, { strict = true })
  return range ~= nil and parsed ~= nil and range:has(parsed)
end

local function names_prerelease(spec)
  local raw = spec.npm_version
  return type(raw) == "string" and raw:match("%d+%.%d+%.%d+%-[%w]") ~= nil
end

function M.select(doc, spec)
  if type(doc) ~= "table" or type(doc.versions) ~= "table" then return nil, "registry document has no versions" end
  local range = range_of(spec)
  if not range then return nil, "invalid version range" end
  local allow_pre = names_prerelease(spec)
  local cands = {}
  for v in pairs(doc.versions) do
    local parsed = vim.version.parse(v, { strict = true })
    if parsed and (allow_pre or parsed.prerelease == nil) and range:has(parsed) then
      cands[#cands + 1] = { v = v, parsed = parsed }
    end
  end
  table.sort(cands, function(a, b) return a.parsed > b.parsed end)
  if #cands == 0 then return nil, "no version satisfies " .. tostring(spec.npm_version or "*") end
  local min_age = M.min_age_days(spec) * DAY
  local held
  local t = now()
  for _, c in ipairs(cands) do
    local published = iso_to_time(doc.time and doc.time[c.v])
    local age = published and (t - published) or nil
    if min_age <= 0 or (age and age >= min_age) then
      return { version = c.v, meta = doc.versions[c.v], held = held }
    end
    held = held or { version = c.v, age = age, min_age = min_age }
  end
  return nil, M.held_text(held), held
end

function M.held_text(held)
  return ("held: %s %s, needs %s"):format(held.version,
    held.age and (M.format_age(math.max(held.age, 0)) .. " old") or "publish time unknown",
    M.format_age(held.min_age))
end

local function decode_json(body)
  local ok, data = pcall(vim.json.decode, body or "", { luanil = { object = true, array = true } })
  if ok and type(data) == "table" then return data end
  return nil
end

function M.parse_provenance(body)
  local data = decode_json(body)
  if not data or type(data.attestations) ~= "table" then return nil end
  for _, a in ipairs(data.attestations) do
    local env = type(a) == "table" and type(a.bundle) == "table" and a.bundle.dsseEnvelope
    if type(a) == "table" and type(a.predicateType) == "string" and a.predicateType:match("^https://slsa%.dev/provenance/")
      and type(env) == "table" and type(env.payload) == "string" then
      local okd, raw = pcall(vim.base64.decode, env.payload)
      local stmt = okd and decode_json(raw)
      local pred = stmt and stmt.predicate
      local wf = type(pred) == "table" and type(pred.buildDefinition) == "table"
        and type(pred.buildDefinition.externalParameters) == "table"
        and pred.buildDefinition.externalParameters.workflow
      if type(wf) == "table" and type(wf.repository) == "string" and type(wf.path) == "string" then
        return { repository = wf.repository, workflow = wf.path }
      end
    end
  end
  return nil
end

function M.provenance_change(locked, candidate)
  local lp = type(locked) == "table" and locked.provenance
  if type(lp) ~= "table" then return nil end
  if type(candidate) ~= "table" then return "provenance missing (locked " .. lp.repository .. ")" end
  if candidate.repository ~= lp.repository then
    return ("provenance repository changed: %s -> %s"):format(lp.repository, candidate.repository)
  end
  if candidate.workflow ~= lp.workflow then
    return ("provenance workflow changed: %s -> %s"):format(lp.workflow, candidate.workflow)
  end
  return nil
end

function M.resolve(spec, cb)
  local pkg = spec.npm
  request(M.REGISTRY .. "/" .. encode_name(pkg), {}, function(err, body)
    if err then return cb(nil, "registry: " .. vim.trim(tostring(err))) end
    local doc = decode_json(body)
    if not doc then return cb(nil, "registry: unreadable document") end
    local pick, perr, held = M.select(doc, spec)
    if not pick then return cb(nil, perr, held) end
    local dist = type(pick.meta) == "table" and pick.meta.dist or {}
    if type(dist.tarball) ~= "string" or type(dist.integrity) ~= "string" then
      return cb(nil, pick.version .. ": registry entry has no tarball or integrity")
    end
    local entry = { npm = pkg, version = pick.version, tarball = dist.tarball, integrity = dist.integrity }
    if dist.attestations == nil then return cb({ entry = entry, held = pick.held }) end
    local url = ("%s/-/npm/v1/attestations/%s@%s"):format(M.REGISTRY, encode_name(pkg), pick.version)
    request(url, {}, function(aerr, abody)
      if aerr then return cb(nil, "attestations: " .. vim.trim(tostring(aerr))) end
      entry.provenance = M.parse_provenance(abody)
      cb({ entry = entry, held = pick.held })
    end)
  end)
end

local function hex_to_base64(hex)
  return vim.base64.encode((hex:gsub("%x%x", function(h) return string.char(tonumber(h, 16)) end)))
end

local HASHERS = {
  { "sha512sum" },
  { "shasum", "-a", "512" },
  { "openssl", "dgst", "-sha512", "-r" },
}

function M.sha512(path, cb)
  for _, h in ipairs(HASHERS) do
    if vim.fn.executable(h[1]) == 1 then
      local cmd = vim.list_extend(vim.deepcopy(h), { path })
      return vim.system(cmd, { text = true }, function(r)
        vim.schedule(function()
          local hex = r.code == 0 and (r.stdout or ""):match("^(%x+)")
          if not hex or #hex ~= 128 then return cb(nil, "sha512 failed: " .. vim.trim(r.stderr or "")) end
          cb("sha512-" .. hex_to_base64(hex))
        end)
      end)
    end
  end
  vim.schedule(function() cb(nil, "no sha512 tool (sha512sum, shasum or openssl) on PATH") end)
end

function M.expected_sha512(integrity)
  for tok in tostring(integrity):gmatch("%S+") do
    if tok:match("^sha512%-") then return (tok:gsub("%?.*$", "")) end
  end
  return nil
end

function M.valid_file(f)
  return type(f) == "string" and f ~= "" and not f:match("^/") and not f:match("^%-")
    and not f:find("[%*%?%[%]\\%z]") and not vim.tbl_contains(vim.split(f, "/", { plain = true }), "..")
end

local function members(files)
  local out = {}
  for _, f in ipairs(files) do
    out[#out + 1] = "package/" .. f:gsub("/+$", "")
  end
  return out
end

local function run(cmd, cb)
  vim.system(cmd, { text = true }, function(r) vim.schedule(function() cb(r) end) end)
end

local function rm_rf(path, cb)
  run({ "rm", "-rf", path }, function() cb() end)
end

local function only_plain(root)
  for name, kind in vim.fs.dir(root, { depth = math.huge }) do
    if kind ~= "file" and kind ~= "directory" then return name end
  end
  return nil
end

function M.read_marker(dir)
  local f = io.open(dir .. "/" .. M.MARKER, "r")
  if not f then return nil end
  local raw = f:read("*a")
  f:close()
  return decode_json(raw)
end

function M.installed_version(dir)
  local m = M.read_marker(dir)
  return m and type(m.version) == "string" and m.version or nil
end

local function random_suffix()
  local b = vim.uv.random(6)
  return b and (b:gsub(".", function(c) return ("%02x"):format(c:byte()) end)) or tostring(vim.uv.hrtime())
end

function M.install(spec, entry, dir, cb)
  local files = spec.files or {}
  if #files == 0 then return vim.schedule(function() cb(false, "spec lists no files") end) end
  for _, f in ipairs(files) do
    if not M.valid_file(f) then return vim.schedule(function() cb(false, "invalid file path: " .. tostring(f)) end) end
  end
  local want = M.expected_sha512(entry.integrity)
  if not want then return vim.schedule(function() cb(false, "lock entry has no sha512 integrity") end) end
  if type(entry.tarball) ~= "string" or not entry.tarball:match("^https://") then
    return vim.schedule(function() cb(false, "tarball URL is not https") end)
  end
  local parent = vim.fs.dirname(dir)
  vim.fn.mkdir(parent, "p")
  local work = ("%s/.core-pack-tmp-%s-%s"):format(parent, vim.fs.basename(dir), random_suffix())
  vim.fn.mkdir(work .. "/stage", "p")
  local tgz = work .. "/package.tgz"
  local function fail(msg)
    rm_rf(work, function() cb(false, msg) end)
  end
  request(entry.tarball, { outpath = tgz }, function(err)
    if err then return fail("download: " .. vim.trim(tostring(err))) end
    M.sha512(tgz, function(got, herr)
      if not got then return fail(herr) end
      if got ~= want then return fail(("integrity mismatch: expected %s, got %s"):format(want, got)) end
      local cmd = { "tar", "-xzf", tgz, "-C", work .. "/stage" }
      vim.list_extend(cmd, members(files))
      run(cmd, function(r)
        if r.code ~= 0 then return fail("extract: " .. vim.trim(r.stderr or ("exit " .. r.code))) end
        local pkgdir = work .. "/stage/package"
        for _, f in ipairs(files) do
          if not vim.uv.fs_lstat(pkgdir .. "/" .. f:gsub("/+$", "")) then return fail("missing in tarball: " .. f) end
        end
        local odd = only_plain(work .. "/stage")
        if odd then return fail("tarball entry is not a regular file or directory: " .. odd) end
        local mf = io.open(pkgdir .. "/" .. M.MARKER, "w")
        if not mf then return fail("cannot write marker") end
        mf:write(vim.json.encode({ npm = entry.npm, version = entry.version, integrity = entry.integrity }))
        mf:close()
        local old
        if vim.uv.fs_lstat(dir) then
          old = work .. "/old"
          local ok, rerr = vim.uv.fs_rename(dir, old)
          if not ok then return fail("move old install: " .. tostring(rerr)) end
        end
        local ok, rerr = vim.uv.fs_rename(pkgdir, dir)
        if not ok then
          if old then vim.uv.fs_rename(old, dir) end
          return fail("move new install: " .. tostring(rerr))
        end
        rm_rf(work, function() cb(true) end)
      end)
    end)
  end)
end

return M
