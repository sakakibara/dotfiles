local T = require("tests.helpers")
local Npm = require("core.pack.npm")

local NOW = 1800000000
local DAY = 86400

local function iso(days_ago)
  return os.date("!%Y-%m-%dT%H:%M:%S.000Z", NOW - math.floor(days_ago * DAY))
end

local function normalize(raw)
  return require("core.pack.spec").normalize(raw)
end

local function doc(versions)
  local d = { versions = {}, time = {} }
  for v, info in pairs(versions) do
    d.versions[v] = { version = v, dist = info.dist or {
      tarball = "https://registry.npmjs.org/pkg/-/pkg-" .. v .. ".tgz",
      integrity = "sha512-" .. v,
      attestations = info.attest and { url = "x" } or nil,
    } }
    d.time[v] = iso(info.age or 30)
  end
  return d
end

local function provenance_body(repo, workflow)
  local stmt = vim.json.encode({
    predicate = { buildDefinition = { externalParameters = { workflow = { repository = repo, path = workflow } } } },
  })
  return vim.json.encode({ attestations = {
    { predicateType = "https://github.com/npm/attestation/tree/main/specs/publish/v0.1", bundle = { dsseEnvelope = { payload = "e30=" } } },
    { predicateType = "https://slsa.dev/provenance/v1", bundle = { dsseEnvelope = { payload = vim.base64.encode(stmt) } } },
  } })
end

local saved = {}
local function stub(routes)
  saved.request, saved.now, saved.min_age = Npm._request, Npm._now, Npm.min_age
  local calls = {}
  Npm._now = function() return NOW end
  Npm._request = function(url, opts, cb)
    calls[#calls + 1] = url
    local r = routes[url]
    vim.schedule(function()
      if r == nil then return cb("404 " .. url) end
      if opts.outpath then
        local i = assert(io.open(r, "rb")); local data = i:read("*a"); i:close()
        local o = assert(io.open(opts.outpath, "wb")); o:write(data); o:close()
        return cb(nil, "")
      end
      cb(nil, type(r) == "table" and vim.json.encode(r) or r)
    end)
  end
  return calls
end

local function unstub()
  Npm._request, Npm._now, Npm.min_age = saved.request, saved.now, saved.min_age
end

local function wait(fn)
  local done, out = false, nil
  fn(function(...) out = { ... }; done = true end)
  vim.wait(20000, function() return done end, 10)
  assert(done, "timed out")
  return unpack(out)
end

local function fixture_tgz(version)
  local root = vim.fn.tempname()
  local p = root .. "/package"
  vim.fn.mkdir(p .. "/dist/sub", "p")
  vim.fn.mkdir(p .. "/lib", "p")
  vim.fn.writefile({ "var v = '" .. version .. "';" }, p .. "/dist/a.js")
  vim.fn.writefile({ "b{}" }, p .. "/dist/sub/b.css")
  vim.fn.writefile({ "readme" }, p .. "/README.md")
  vim.fn.writefile({ "lib" }, p .. "/lib/x.js")
  local tgz = root .. "/pkg.tgz"
  local r = vim.system({ "tar", "-czf", tgz, "-C", root, "package" }):wait()
  assert(r.code == 0, r.stderr)
  local integrity = wait(function(cb) Npm.sha512(tgz, cb) end)
  return tgz, integrity
end

T.describe("core.pack.npm resolution", function()
  T.it("picks the highest version in range and skips prereleases", function()
    local d = doc({ ["0.18.9"] = {}, ["0.19.0"] = {}, ["0.19.2"] = {}, ["0.19.3-rc.1"] = {}, ["0.20.0"] = {} })
    Npm._now = function() return NOW end
    local pick = Npm.select(d, normalize({ npm = "pkg", version = "^0.19", files = { "dist" }, min_age = 0 }))
    T.eq(pick.version, "0.19.2")
    pick = Npm.select(d, normalize({ npm = "pkg", version = "~0.19.3-rc.1", files = { "dist" }, min_age = 0 }))
    T.eq(pick.version, "0.19.3-rc.1")
    local none, err = Npm.select(d, normalize({ npm = "pkg", version = "^1", files = { "dist" }, min_age = 0 }))
    T.eq(none, nil)
    T.truthy(err:match("no version"))
    Npm._now = nil
  end)

  T.it("holds back versions younger than min_age and reports them", function()
    local d = doc({ ["0.19.0"] = { age = 30 }, ["0.19.2"] = { age = 2.5 } })
    Npm._now = function() return NOW end
    local spec = normalize({ npm = "pkg", version = "^0.19", files = { "dist" } })
    local pick = Npm.select(d, spec)
    T.eq(pick.version, "0.19.0")
    T.eq(Npm.held_text(pick.held), "held: 0.19.2 2d old, needs 7d")
    Npm.min_age = 0
    T.eq(Npm.select(d, spec).version, "0.19.2")
    Npm.min_age = nil
    T.eq(Npm.select(d, normalize({ npm = "pkg", version = "^0.19", files = { "dist" }, min_age = 2 })).version, "0.19.2")
    local young = doc({ ["0.19.2"] = { age = 1 } })
    local none, err = Npm.select(young, spec)
    T.eq(none, nil)
    T.eq(err, "held: 0.19.2 1d old, needs 7d")
    Npm._now = nil
  end)

  T.it("records SLSA provenance from the attestations endpoint", function()
    local d = doc({ ["1.0.0"] = { attest = true } })
    stub({
      ["https://registry.npmjs.org/@scope%2fpkg"] = d,
      ["https://registry.npmjs.org/-/npm/v1/attestations/@scope%2fpkg@1.0.0"] =
        provenance_body("https://github.com/o/r", ".github/workflows/release.yml"),
    })
    local res, err = wait(function(cb) Npm.resolve(normalize({ npm = "@scope/pkg", files = { "dist" } }), cb) end)
    unstub()
    T.eq(err, nil)
    T.eq(res.entry.version, "1.0.0")
    T.eq(res.entry.provenance, { repository = "https://github.com/o/r", workflow = ".github/workflows/release.yml" })
    T.eq(normalize({ npm = "@scope/pkg", files = { "dist" } }).name, "scope-pkg")
  end)

  T.it("flags a candidate whose provenance is missing or changed", function()
    local locked = { provenance = { repository = "https://github.com/o/r", workflow = "w.yml" } }
    T.eq(Npm.provenance_change(locked, { repository = "https://github.com/o/r", workflow = "w.yml" }), nil)
    T.truthy(Npm.provenance_change(locked, nil):match("missing"))
    T.truthy(Npm.provenance_change(locked, { repository = "https://github.com/x/r", workflow = "w.yml" }):match("repository"))
    T.truthy(Npm.provenance_change(locked, { repository = "https://github.com/o/r", workflow = "x.yml" }):match("workflow"))
    T.eq(Npm.provenance_change({}, nil), nil)
  end)

  T.it("rejects invalid names and file paths in a spec", function()
    T.truthy(not pcall(normalize, { npm = "../x", files = { "dist" } }))
    T.truthy(not pcall(normalize, { npm = "x", files = { "../etc" } }))
    T.truthy(not pcall(normalize, { npm = "x", files = { "/abs" } }))
    T.truthy(not pcall(normalize, { npm = "x", files = {} }))
  end)

  T.it("rejects npm version ranges the resolver would misread", function()
    for _, v in ipairs({ ">=12 <13", "^11 || ^12", "1.0.0 - 2.0.0", "^11||^12", ">=1.2.3  <2" }) do
      local ok, err = pcall(normalize, { npm = "pkg", version = v, files = { "dist" } })
      T.eq(ok, false, v)
      T.truthy(err:find("npm spec pkg: unsupported version", 1, true), err)
      T.truthy(err:find("single comparator", 1, true), err)
    end
    for _, v in ipairs({ "1.2.3", "^1.2", "~1.2.3", "1.x", "*", ">=12", ">= 12", " ^1 " }) do
      T.truthy(pcall(normalize, { npm = "pkg", version = v, files = { "dist" } }), v)
    end
  end)
end)

T.describe("core.pack.npm install", function()
  local function root()
    local r = vim.fn.tempname() .. "-npm"
    vim.fn.mkdir(r, "p")
    return r
  end

  T.it("refuses a tarball whose sha512 does not match the integrity", function()
    local tgz = fixture_tgz("1.0.0")
    local base = root()
    local url = "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz"
    stub({ [url] = tgz })
    local entry = { npm = "pkg", version = "1.0.0", tarball = url, integrity = "sha512-" .. vim.base64.encode(("x"):rep(64)) }
    local ok, err = wait(function(cb) Npm.install({ npm = "pkg", files = { "dist" } }, entry, base .. "/pkg", cb) end)
    unstub()
    T.eq(ok, false)
    T.truthy(err:match("integrity mismatch"), err)
    T.eq(vim.fn.isdirectory(base .. "/pkg"), 0)
    T.eq(vim.fn.readdir(base), {})
  end)

  T.it("extracts only the listed files and records the version", function()
    local tgz, integrity = fixture_tgz("1.0.0")
    local base = root()
    local url = "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz"
    stub({ [url] = tgz })
    local entry = { npm = "pkg", version = "1.0.0", tarball = url, integrity = integrity }
    local ok, err = wait(function(cb) Npm.install({ npm = "pkg", files = { "dist", "lib/x.js" } }, entry, base .. "/pkg", cb) end)
    unstub()
    T.eq(err, nil)
    T.eq(ok, true)
    local dir = base .. "/pkg"
    T.eq(vim.fn.filereadable(dir .. "/dist/a.js"), 1)
    T.eq(vim.fn.filereadable(dir .. "/dist/sub/b.css"), 1)
    T.eq(vim.fn.filereadable(dir .. "/lib/x.js"), 1)
    T.eq(vim.fn.filereadable(dir .. "/README.md"), 0)
    T.eq(Npm.installed_version(dir), "1.0.0")
    T.eq(vim.fn.readdir(base), { "pkg" })
  end)

  T.it("replaces an existing install as a whole", function()
    local base = root()
    local dir = base .. "/pkg"
    for _, v in ipairs({ "1.0.0", "1.1.0" }) do
      local tgz, integrity = fixture_tgz(v)
      local url = "https://registry.npmjs.org/pkg/-/pkg-" .. v .. ".tgz"
      stub({ [url] = tgz })
      local ok = wait(function(cb)
        Npm.install({ npm = "pkg", files = { "dist" } }, { npm = "pkg", version = v, tarball = url, integrity = integrity }, dir, cb)
      end)
      unstub()
      T.eq(ok, true)
    end
    T.eq(Npm.installed_version(dir), "1.1.0")
    T.eq(vim.fn.readfile(dir .. "/dist/a.js"), { "var v = '1.1.0';" })
    T.eq(vim.fn.readdir(base), { "pkg" })
  end)

  T.it("refuses a listed path the tarball does not have", function()
    local tgz, integrity = fixture_tgz("1.0.0")
    local base = root()
    local url = "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz"
    stub({ [url] = tgz })
    local ok = wait(function(cb)
      Npm.install({ npm = "pkg", files = { "nope" } }, { npm = "pkg", version = "1.0.0", tarball = url, integrity = integrity }, base .. "/pkg", cb)
    end)
    unstub()
    T.eq(ok, false)
    T.eq(vim.fn.readdir(base), {})
  end)
end)

T.describe("core.pack npm plugins", function()
  local function fresh()
    package.loaded["core.pack.install"] = nil
    package.loaded["core.pack"] = nil
    package.loaded["core.event"] = nil
    local I = require("core.pack.install")
    local L = require("core.pack.lock")
    L._path_override = vim.fn.tempname() .. ".json"
    require("core.pack.log")._path_override = vim.fn.tempname() .. ".jsonl"
    I._install_root_override = vim.fn.tempname() .. "-pack"
    vim.fn.mkdir(I._install_root_override, "p")
    return I, L
  end

  local function async(fn)
    local done = false
    fn(function() done = true end)
    vim.wait(30000, function() return done end, 10)
    assert(done, "timed out")
  end

  local SPEC = { npm = "pkg", version = "^1", files = { "dist" } }

  local function routes(versions, tgzs)
    local d = doc(versions)
    local r = { ["https://registry.npmjs.org/pkg"] = d }
    for v, t in pairs(tgzs) do
      d.versions[v].dist.integrity = t.integrity
      r[d.versions[v].dist.tarball] = t.tgz
      if t.prov then
        d.versions[v].dist.attestations = { url = "x" }
        r["https://registry.npmjs.org/-/npm/v1/attestations/pkg@" .. v] = provenance_body(t.prov, "w.yml")
      end
    end
    return r
  end

  local function tgz(v, prov)
    local file, integrity = fixture_tgz(v)
    return { tgz = file, integrity = integrity, prov = prov }
  end

  T.it("installs from the registry, locks it, and never puts it on the runtimepath", function()
    local I, L = fresh()
    stub(routes({ ["1.0.0"] = {} }, { ["1.0.0"] = tgz("1.0.0") }))
    local pack = require("core.pack")
    pack.setup({ specs = { SPEC } })
    unstub()
    local dir = I.install_dir("pkg")
    local e = L.get("pkg")
    T.eq({ e.npm, e.version, e.tarball }, { "pkg", "1.0.0", "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz" })
    T.truthy(e.integrity:match("^sha512%-"))
    T.eq(Lib.plugin.dir("pkg"), dir)
    pack.load("pkg")
    T.eq(pack.loaded("pkg"), false)
    T.eq(vim.tbl_contains(vim.api.nvim_list_runtime_paths(), dir), false)
  end)

  T.it("installs a locked entry without consulting the registry or the age gate", function()
    local I, L = fresh()
    local t = tgz("1.0.0")
    local url = "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz"
    L.set("pkg", { npm = "pkg", version = "1.0.0", tarball = url, integrity = t.integrity })
    local calls = stub({ [url] = t.tgz })
    async(function(cb) I.install_missing({ normalize(SPEC) }, { on_complete = cb }) end)
    unstub()
    T.eq(calls, { url })
    T.eq(require("core.pack.npm").installed_version(I.install_dir("pkg")), "1.0.0")
  end)

  T.it("update applies a newer version, keeps provenance, and reports one held by the age gate", function()
    local I, L = fresh()
    local spec = normalize(SPEC)
    stub(routes({ ["1.0.0"] = { age = 40 } }, { ["1.0.0"] = tgz("1.0.0", "https://github.com/o/r") }))
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    unstub()
    T.eq(L.get("pkg").provenance, { repository = "https://github.com/o/r", workflow = "w.yml" })
    stub(routes({ ["1.0.0"] = { age = 40 }, ["1.1.0"] = { age = 10 }, ["1.2.0"] = { age = 1 } },
      { ["1.1.0"] = tgz("1.1.0", "https://github.com/o/r") }))
    local warns = {}
    local orig = vim.notify
    vim.notify = function(m, l) if l == vim.log.levels.WARN then warns[#warns + 1] = m end end
    async(function(cb) I.update({ spec }, { "pkg" }, { confirm = false, open_window = false, on_complete = cb }) end)
    vim.notify = orig
    unstub()
    T.eq(L.get("pkg").version, "1.1.0")
    T.eq(require("core.pack.npm").installed_version(I.install_dir("pkg")), "1.1.0")
    T.truthy(table.concat(warns, "\n"):find("pkg: held: 1.2.0 1d old, needs 7d", 1, true), vim.inspect(warns))
  end)

  T.it("update review lists the held version and blocks a provenance change", function()
    local I, L = fresh()
    local spec = normalize(SPEC)
    stub(routes({ ["1.0.0"] = { age = 40 } }, { ["1.0.0"] = tgz("1.0.0", "https://github.com/o/r") }))
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    unstub()
    stub(routes({ ["1.0.0"] = { age = 40 }, ["1.1.0"] = { age = 10 }, ["1.2.0"] = { age = 1 } },
      { ["1.1.0"] = tgz("1.1.0", "https://github.com/evil/r") }))
    local UI = require("core.pack.ui")
    local orig_review = UI.update_review
    local seen
    UI.update_review = function(items, opts) seen = { items = items, notices = opts.notices } end
    local orig = vim.notify
    vim.notify = function() end
    local done = false
    I.update({ spec }, { "pkg" }, { open_window = false, on_complete = function() done = true end })
    vim.wait(20000, function() return done or seen ~= nil end, 10)
    vim.notify = orig
    UI.update_review = orig_review
    unstub()
    T.eq(seen, nil, "a blocked candidate leaves nothing to review")
    T.eq(L.get("pkg").version, "1.0.0")

    stub(routes({ ["1.0.0"] = { age = 40 }, ["1.1.0"] = { age = 10 }, ["1.2.0"] = { age = 1 } },
      { ["1.1.0"] = tgz("1.1.0", "https://github.com/o/r") }))
    UI.update_review = function(items, opts) seen = { items = items, notices = opts.notices } end
    I.update({ spec }, { "pkg" }, { open_window = false })
    vim.wait(20000, function() return seen ~= nil end, 10)
    UI.update_review = orig_review
    unstub()
    T.eq(#seen.items, 1)
    T.eq({ seen.items[1].from, seen.items[1].to, seen.items[1].npm }, { "1.0.0", "1.1.0", true })
    T.eq(seen.notices, { { name = "pkg", kind = "held", text = "held: 1.2.0 1d old, needs 7d" } })
  end)

  T.it("update refuses a candidate whose provenance repository changed", function()
    local I, L = fresh()
    local spec = normalize(SPEC)
    stub(routes({ ["1.0.0"] = { age = 40 } }, { ["1.0.0"] = tgz("1.0.0", "https://github.com/o/r") }))
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    unstub()
    stub(routes({ ["1.0.0"] = { age = 40 }, ["1.1.0"] = { age = 10 } }, { ["1.1.0"] = tgz("1.1.0", "https://github.com/evil/r") }))
    local warns = {}
    local orig = vim.notify
    vim.notify = function(m, l) if l == vim.log.levels.WARN then warns[#warns + 1] = m end end
    async(function(cb) I.update({ spec }, { "pkg" }, { confirm = false, open_window = false, on_complete = cb }) end)
    vim.notify = orig
    unstub()
    T.eq(L.get("pkg").version, "1.0.0")
    T.truthy(table.concat(warns, "\n"):find("provenance repository changed", 1, true), vim.inspect(warns))
  end)

  T.it("reconcile reports an installed version that differs from the lock", function()
    local I, L = fresh()
    local spec = normalize(SPEC)
    stub(routes({ ["1.0.0"] = {} }, { ["1.0.0"] = tgz("1.0.0") }))
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    unstub()
    local e = L.get("pkg")
    e.version = "1.0.1"
    L.set("pkg", e)
    local drifts
    I.reconcile({ spec }, function(d) drifts = d end)
    vim.wait(5000, function() return drifts ~= nil end, 10)
    T.eq(drifts, { { name = "pkg", expected = "1.0.1", actual = "1.0.0" } })
  end)
end)

T.describe("core.pack.lock nested entries", function()
  T.it("round-trips an npm entry with provenance byte-for-byte", function()
    local L = require("core.pack.lock")
    local prev = L._path_override
    L._path_override = vim.fn.tempname() .. ".json"
    local entry = { npm = "pkg", version = "1.0.0", tarball = "https://x/pkg.tgz", integrity = "sha512-abc",
      provenance = { workflow = "w.yml", repository = "https://github.com/o/r" } }
    L.set("pkg", entry)
    local first = table.concat(vim.fn.readfile(L.path()), "\n")
    L.set("git", { src = "https://github.com/a/b", rev = ("a"):rep(40) })
    L.delete("git")
    T.eq(L.get("pkg"), entry)
    T.eq(table.concat(vim.fn.readfile(L.path()), "\n"), first)
    T.truthy(first:find('"provenance":{"repository":"https://github.com/o/r","workflow":"w.yml"}', 1, true))
    L._path_override = prev
  end)
end)
