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

local function tar_entry(name, mode, typeflag, body, link)
  body = body or ""
  local function oct(n, w) return ("%0" .. (w - 1) .. "o"):format(n) .. "\0" end
  local function pad(str, w) return str .. ("\0"):rep(w - #str) end
  local h = pad(name, 100) .. oct(mode, 8) .. oct(0, 8) .. oct(0, 8) .. oct(#body, 12) .. oct(NOW, 12)
    .. "        " .. typeflag .. pad(link or "", 100) .. "ustar\0" .. "00" .. ("\0"):rep(247)
  local sum = 0
  for i = 1, #h do sum = sum + h:byte(i) end
  h = h:sub(1, 148) .. ("%06o"):format(sum) .. "\0 " .. h:sub(157)
  return h .. body .. ("\0"):rep((512 - #body % 512) % 512)
end

local function crafted_tgz(entries)
  local parts = {}
  for _, e in ipairs(entries) do parts[#parts + 1] = tar_entry(e[1], e[2], e[3], e[4], e[5]) end
  parts[#parts + 1] = ("\0"):rep(1024)
  local r = vim.system({ "gzip", "-c", "-n" }, { stdin = table.concat(parts) }):wait()
  assert(r.code == 0, r.stderr)
  local tgz = vim.fn.tempname() .. ".tgz"
  local f = assert(io.open(tgz, "wb")); f:write(r.stdout); f:close()
  local integrity
  local done = false
  Npm.sha512(tgz, function(got) integrity = got; done = true end)
  vim.wait(10000, function() return done end, 10)
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

  T.it("does not pick a version above dist-tags.latest when the range admits latest", function()
    local d = doc({ ["10.9.0"] = {}, ["11.4.0"] = {}, ["11.5.0"] = {}, ["12.0.0"] = {} })
    d["dist-tags"] = { latest = "11.4.0", next = "12.0.0" }
    Npm._now = function() return NOW end
    local function pick(v)
      return Npm.select(d, normalize({ npm = "pkg", version = v, files = { "dist" }, min_age = 0 })).version
    end
    T.eq(pick(nil), "11.4.0")
    T.eq(pick("*"), "11.4.0")
    T.eq(pick(">=10"), "11.4.0")
    T.eq(pick("^11"), "11.4.0")
    T.eq(pick("^12"), "12.0.0")
    T.eq(pick("~10.9"), "10.9.0")
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

  T.it("reads registry publish times as UTC in any local time zone", function()
    local script = vim.fn.tempname() .. ".lua"
    vim.fn.writefile({
      "vim.opt.rtp:prepend(" .. vim.inspect(vim.fn.getcwd()) .. ")",
      "local N = require('core.pack.npm')",
      "io.stdout:write(N.iso_to_time('2026-07-01T12:00:00.000Z'), ' ', N.iso_to_time('2026-01-15T03:04:05Z'))",
    }, script)
    for _, tz in ipairs({ "America/New_York", "Asia/Tokyo", "UTC" }) do
      local r = vim.system({ vim.v.progpath, "--headless", "--clean", "-l", script }, { env = { TZ = tz }, text = true })
        :wait(10000)
      T.eq(vim.trim(r.stdout or ""), "1782907200 1768446245", tz)
    end
    vim.fn.delete(script)
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

  T.it("hashes with every available tool, whatever the path contains", function()
    local dir = vim.fn.tempname()
    vim.fn.mkdir(dir, "p")
    local path = dir .. "/we\\ird\n -name.tgz"
    local f = assert(io.open(path, "wb")); f:write("abc"); f:close()
    local want = "sha512-" .. vim.base64.encode((("ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a"
      .. "2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"):gsub("%x%x", function(x)
        return string.char(tonumber(x, 16))
      end)))
    local all = Npm._hashers
    local tried = 0
    for _, h in ipairs(all) do
      if vim.fn.executable(h.cmd[1]) == 1 then
        tried = tried + 1
        Npm._hashers = { h }
        local got, err = wait(function(cb) Npm.sha512(path, cb) end)
        T.eq({ got, err }, { want, nil }, h.cmd[1])
      end
    end
    Npm._hashers = all
    T.truthy(tried > 0, "no sha512 tool available")
    vim.fn.delete(dir, "rf")
  end)

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

  T.it("refuses a link hidden in an unreadable directory and leaves nothing behind", function()
    local tgz, integrity = crafted_tgz({
      { "package/", 493, "5" },
      { "package/dist/", 493, "5" },
      { "package/dist/a.js", 420, "0", "a\n" },
      { "package/dist/z/", 0, "5" },
      { "package/dist/z/l", 420, "2", nil, "/etc/passwd" },
    })
    local base = root()
    local url = "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz"
    stub({ [url] = tgz })
    local ok, err = wait(function(cb)
      Npm.install({ npm = "pkg", files = { "dist" } }, { npm = "pkg", version = "1.0.0", tarball = url, integrity = integrity }, base .. "/pkg", cb)
    end)
    unstub()
    T.eq(ok, false)
    T.truthy(tostring(err):find("not a regular file or directory: package/dist/z/l", 1, true), err)
    T.eq(vim.fn.readdir(base), {})
  end)

  T.it("opens up unreadable directories and files so the install can be read and removed", function()
    local tgz, integrity = crafted_tgz({
      { "package/", 493, "5" },
      { "package/dist/", 0, "5" },
      { "package/dist/z/", 0, "5" },
      { "package/dist/z/a.js", 0, "0", "a\n" },
    })
    local base = root()
    local url = "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz"
    stub({ [url] = tgz })
    local ok, err = wait(function(cb)
      Npm.install({ npm = "pkg", files = { "dist" } }, { npm = "pkg", version = "1.0.0", tarball = url, integrity = integrity }, base .. "/pkg", cb)
    end)
    unstub()
    T.eq({ ok, err }, { true, nil })
    T.eq(vim.fn.readfile(base .. "/pkg/dist/z/a.js"), { "a" })
    T.eq(vim.fn.readdir(base), { "pkg" })
    vim.fs.rm(base, { recursive = true })
    T.eq(vim.uv.fs_lstat(base), nil)
  end)

  T.it("concurrent installs into one directory run one at a time and all succeed", function()
    local tgz, integrity = fixture_tgz("1.0.0")
    local base = root()
    local url = "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz"
    local calls = stub({ [url] = tgz })
    local entry = { npm = "pkg", version = "1.0.0", tarball = url, integrity = integrity }
    local results = {}
    for i = 1, 3 do
      Npm.install({ npm = "pkg", files = { "dist" } }, entry, base .. "/pkg", function(ok, err) results[i] = { ok, err } end)
    end
    vim.wait(20000, function() return #results == 3 and results[1] and results[2] and results[3] end, 10)
    unstub()
    T.eq(results, { { true, nil }, { true, nil }, { true, nil } })
    T.eq(calls, { url }, "a queued install of what is already in place does not download again")
    T.eq(vim.fn.readdir(base), { "pkg" })
    T.eq(Npm.installed_version(base .. "/pkg"), "1.0.0")
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
    T.eq(drifts, { { name = "pkg", reason = "installed 1.0.0, lockfile 1.0.1" } })
  end)

  local function marker(I)
    return require("core.pack.npm").read_marker(I.install_dir("pkg"))
  end

  T.it("reinstalls when the spec's files change, and reconcile reports the mismatch", function()
    local I, L = fresh()
    local t = tgz("1.0.0")
    stub(routes({ ["1.0.0"] = {} }, { ["1.0.0"] = t }))
    async(function(cb) I.install_missing({ normalize(SPEC) }, { on_complete = cb }) end)
    unstub()
    local m = marker(I)
    T.eq({ m.npm, m.version, m.integrity, m.files }, { "pkg", "1.0.0", t.integrity, { "dist" } })
    local wider = normalize({ npm = "pkg", version = "^1", files = { "lib/x.js", "dist/" } })
    local drifts
    I.reconcile({ wider }, function(d) drifts = d end)
    vim.wait(5000, function() return drifts ~= nil end, 10)
    T.eq(drifts, { { name = "pkg", reason = "installed files differ from the spec" } })
    local calls = stub(routes({ ["1.0.0"] = {} }, { ["1.0.0"] = t }))
    async(function(cb) I.install_missing({ wider }, { on_complete = cb }) end)
    unstub()
    T.eq(calls, { "https://registry.npmjs.org/pkg/-/pkg-1.0.0.tgz" })
    T.eq(marker(I).files, { "dist", "lib/x.js" })
    T.eq(vim.fn.filereadable(I.install_dir("pkg") .. "/lib/x.js"), 1)
    calls = stub({})
    async(function(cb) I.install_missing({ normalize({ npm = "pkg", version = "^1", files = { "dist", "lib/x.js", "dist" } }) },
      { on_complete = cb }) end)
    unstub()
    T.eq(calls, {}, "an equivalent files list is not a change")
    T.eq(L.get("pkg").version, "1.0.0")
  end)

  T.it("reinstalls when the npm package changes under an explicit name", function()
    local I, L = fresh()
    stub(routes({ ["1.0.0"] = {} }, { ["1.0.0"] = tgz("1.0.0") }))
    async(function(cb) I.install_missing({ normalize({ npm = "pkg", name = "pkg", files = { "dist" } }) }, { on_complete = cb }) end)
    unstub()
    local other = normalize({ npm = "other", name = "pkg", files = { "dist" } })
    local drifts
    I.reconcile({ other }, function(d) drifts = d end)
    vim.wait(5000, function() return drifts ~= nil end, 10)
    T.eq(drifts, { { name = "pkg", reason = "installed package pkg, spec other" } })
    local t = tgz("2.0.0")
    local d = doc({ ["2.0.0"] = {} })
    d.versions["2.0.0"].dist = { tarball = "https://registry.npmjs.org/other/-/other-2.0.0.tgz", integrity = t.integrity }
    stub({ ["https://registry.npmjs.org/other"] = d, ["https://registry.npmjs.org/other/-/other-2.0.0.tgz"] = t.tgz })
    async(function(cb) I.install_missing({ other }, { on_complete = cb }) end)
    unstub()
    T.eq({ marker(I).npm, marker(I).version }, { "other", "2.0.0" })
    T.eq({ L.get("pkg").npm, L.get("pkg").version }, { "other", "2.0.0" })
  end)

  T.it("startup reinstalls an npm plugin whose installed files differ from the spec", function()
    local I = fresh()
    local t = tgz("1.0.0")
    stub(routes({ ["1.0.0"] = {} }, { ["1.0.0"] = t }))
    require("core.pack").setup({ specs = { SPEC } })
    unstub()
    T.eq(marker(I).files, { "dist" })
    package.loaded["core.pack"] = nil
    stub(routes({ ["1.0.0"] = {} }, { ["1.0.0"] = t }))
    local orig = vim.notify
    vim.notify = function() end
    require("core.pack").setup({ specs = { { npm = "pkg", version = "^1", files = { "dist", "lib" } } } })
    vim.notify = orig
    unstub()
    T.eq(marker(I).files, { "dist", "lib" })
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
