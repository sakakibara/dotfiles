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

local function days_from_civil(y, m, d)
  if m <= 2 then y = y - 1 end
  local era = math.floor(y / 400)
  local yoe = y - era * 400
  local doy = math.floor((153 * ((m + 9) % 12) + 2) / 5) + d - 1
  local doe = yoe * 365 + math.floor(yoe / 4) - math.floor(yoe / 100) + doy
  return era * 146097 + doe - 719468
end

function M.iso_to_time(s)
  if type(s) ~= "string" then return nil end
  local y, mo, d, h, mi, sec = s:match("^(%d+)%-(%d+)%-(%d+)T(%d+):(%d+):(%d+)")
  if not y then return nil end
  return days_from_civil(tonumber(y), tonumber(mo), tonumber(d)) * DAY
    + tonumber(h) * 3600 + tonumber(mi) * 60 + tonumber(sec)
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

function M.newer(a, b)
  if type(a) ~= "string" then return false end
  if type(b) ~= "string" then return true end
  local pa = vim.version.parse(a, { strict = true })
  local pb = vim.version.parse(b, { strict = true })
  if not (pa and pb) then return a ~= b end
  return pb < pa
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
  local tags = type(doc["dist-tags"]) == "table" and doc["dist-tags"] or {}
  local latest = type(tags.latest) == "string" and vim.version.parse(tags.latest, { strict = true }) or nil
  if latest and not range:has(latest) then latest = nil end
  local cands = {}
  for v in pairs(doc.versions) do
    local parsed = vim.version.parse(v, { strict = true })
    if parsed and (allow_pre or parsed.prerelease == nil) and range:has(parsed)
      and not (latest and parsed > latest) then
      cands[#cands + 1] = { v = v, parsed = parsed }
    end
  end
  table.sort(cands, function(a, b) return a.parsed > b.parsed end)
  if #cands == 0 then return nil, "no version satisfies " .. tostring(spec.npm_version or "*") end
  local min_age = M.min_age_days(spec) * DAY
  local held
  local t = now()
  for _, c in ipairs(cands) do
    local published = M.iso_to_time(doc.time and doc.time[c.v])
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

M._hashers = {
  { cmd = { "shasum", "-a", "512" } },
  { cmd = { "sha512sum" } },
  { cmd = { "openssl", "dgst", "-sha512", "-binary" }, binary = true },
}

local function digest_of(h, out)
  if h.binary then
    return #out == 64 and out or nil
  end
  local hex = out:match("^(%x+) [ %*]%-\n$")
  if not hex or #hex ~= 128 then return nil end
  return (hex:gsub("%x%x", function(x) return string.char(tonumber(x, 16)) end))
end

function M.sha512(path, cb)
  local h
  for _, cand in ipairs(M._hashers) do
    if vim.fn.executable(cand.cmd[1]) == 1 then h = cand; break end
  end
  if not h then
    return vim.schedule(function() cb(nil, "no sha512 tool (shasum, sha512sum or openssl) on PATH") end)
  end
  local f = io.open(path, "rb")
  if not f then return vim.schedule(function() cb(nil, "sha512: cannot read " .. path) end) end
  local data = f:read("*a")
  f:close()
  vim.system(h.cmd, { stdin = data }, function(r)
    vim.schedule(function()
      local raw = r.code == 0 and digest_of(h, r.stdout or "") or nil
      if not raw then return cb(nil, ("sha512 failed (%s): %s"):format(h.cmd[1], vim.trim(r.stderr or ""))) end
      cb("sha512-" .. vim.base64.encode(raw))
    end)
  end)
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

function M.norm_files(files)
  local seen, out = {}, {}
  for _, f in ipairs(files or {}) do
    local n = (f:gsub("/+$", ""))
    if not seen[n] then
      seen[n] = true
      out[#out + 1] = n
    end
  end
  table.sort(out)
  return out
end

local function members(files)
  local out = {}
  for _, f in ipairs(M.norm_files(files)) do out[#out + 1] = "package/" .. f end
  return out
end

local function run(cmd, cb)
  vim.system(cmd, { text = true }, function(r) vim.schedule(function() cb(r) end) end)
end

local function perm(st)
  return bit.band(st.mode, 4095)
end

local function grant(path, st, bits)
  local want = bit.bor(perm(st), bits)
  if want == perm(st) then return true end
  return vim.uv.fs_chmod(path, want) ~= nil
end

local function entries(dir)
  local handle, err = vim.uv.fs_scandir(dir)
  if not handle then return nil, err end
  local out = {}
  while true do
    local name, kind, nerr = vim.uv.fs_scandir_next(handle)
    if not name then
      if kind then return nil, kind end
      if nerr then return nil, nerr end
      break
    end
    out[#out + 1] = name
  end
  return out
end

local function remove_tree(path)
  local st = vim.uv.fs_lstat(path)
  if not st then return true end
  local function loosen(dir, dst)
    grant(dir, dst, 448)
    for _, name in ipairs(entries(dir) or {}) do
      local p = dir .. "/" .. name
      local est = vim.uv.fs_lstat(p)
      if est and est.type == "directory" then loosen(p, est) end
    end
  end
  if st.type == "directory" then loosen(path, st) end
  local ok, err = pcall(vim.fs.rm, path, { recursive = true, force = true })
  if ok and not vim.uv.fs_lstat(path) then return true end
  return false, tostring(err or "still present")
end

local function plain_tree(root)
  local stack = { "" }
  while #stack > 0 do
    local rel = table.remove(stack)
    local names, err = entries(rel == "" and root or root .. "/" .. rel)
    if not names then return ("cannot scan %s: %s"):format(rel == "" and "." or rel, tostring(err)) end
    for _, name in ipairs(names) do
      local r = rel == "" and name or rel .. "/" .. name
      local p = root .. "/" .. r
      local st = vim.uv.fs_lstat(p)
      if not st then return "cannot stat " .. r end
      if st.type == "directory" then
        if not grant(p, st, 448) then return "cannot open " .. r end
        stack[#stack + 1] = r
      elseif st.type == "file" then
        if not grant(p, st, 384) then return "cannot open " .. r end
      else
        return "tarball entry is not a regular file or directory: " .. r
      end
    end
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

local function write_marker(dir, m)
  local f = io.open(dir .. "/" .. M.MARKER, "w")
  if not f then return false end
  f:write(vim.json.encode(m))
  f:close()
  return true
end

local function adopt_legacy(spec, dir, m, locked)
  if m.files ~= nil or not M.usable_lock(spec, locked) then return false end
  if m.version ~= locked.version or m.integrity ~= locked.integrity then return false end
  local files = M.norm_files(spec.files)
  for _, f in ipairs(files) do
    if not vim.uv.fs_lstat(dir .. "/" .. f) then return false end
  end
  m.files = files
  return write_marker(dir, m)
end

function M.mismatch(spec, dir, locked)
  local m = M.read_marker(dir)
  if not m or type(m.version) ~= "string" then return "not installed" end
  if m.npm ~= spec.npm then return ("installed package %s, spec %s"):format(tostring(m.npm), spec.npm) end
  if m.files == nil then adopt_legacy(spec, dir, m, locked) end
  if not vim.deep_equal(m.files, M.norm_files(spec.files)) then return "installed files differ from the spec" end
  if M.usable_lock(spec, locked) then
    if m.version ~= locked.version then return ("installed %s, lockfile %s"):format(m.version, locked.version) end
    if m.integrity ~= locked.integrity then return ("installed %s integrity differs from the lockfile"):format(m.version) end
    return nil
  end
  local range = range_of(spec)
  local parsed = vim.version.parse(m.version, { strict = true })
  if not (range and parsed and range:has(parsed)) then
    return ("installed %s is outside the spec's version range"):format(m.version)
  end
  return nil
end

local function random_suffix()
  local b = vim.uv.random(6)
  return b and (b:gsub(".", function(c) return ("%02x"):format(c:byte()) end)) or tostring(vim.uv.hrtime())
end

local function install_one(spec, entry, dir, cb)
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
    local ok, err = remove_tree(work)
    cb(false, ok and msg or ("%s; could not remove %s: %s"):format(msg, work, err))
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
        local odd = plain_tree(work .. "/stage")
        if odd then return fail(odd) end
        for _, f in ipairs(files) do
          if not vim.uv.fs_lstat(pkgdir .. "/" .. f:gsub("/+$", "")) then return fail("missing in tarball: " .. f) end
        end
        local mf = io.open(pkgdir .. "/" .. M.MARKER, "w")
        if not mf then return fail("cannot write marker") end
        mf:write(vim.json.encode({
          npm = entry.npm, version = entry.version, integrity = entry.integrity, files = M.norm_files(files),
        }))
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
        local cok, cerr = remove_tree(work)
        if not cok then
          vim.notify(("core.pack: could not remove %s: %s"):format(work, cerr), vim.log.levels.WARN)
        end
        cb(true)
      end)
    end)
  end)
end

local queues = {}

local function installed_as(spec, entry, dir)
  local m = M.read_marker(dir)
  return m ~= nil and m.npm == entry.npm and m.version == entry.version and m.integrity == entry.integrity
    and vim.deep_equal(m.files, M.norm_files(spec.files))
end

function M.install(spec, entry, dir, cb)
  if queues[dir] then
    table.insert(queues[dir], { spec, entry, cb })
    return
  end
  queues[dir] = {}
  local step
  local function advance()
    local nxt = table.remove(queues[dir], 1)
    while nxt and installed_as(nxt[1], nxt[2], dir) do
      local done = nxt[3]
      vim.schedule(function() done(true) end)
      nxt = table.remove(queues[dir], 1)
    end
    if nxt then step(nxt[1], nxt[2], nxt[3]) else queues[dir] = nil end
  end
  step = function(s, e, c)
    local finished = false
    local function finish(...)
      if finished then return end
      finished = true
      advance()
      c(...)
    end
    local ok, err = pcall(install_one, s, e, dir, finish)
    if not ok then
      local msg = tostring(err)
      vim.schedule(function() finish(false, msg) end)
    end
  end
  step(spec, entry, cb)
end

return M
