local T = require("tests.helpers")

local function sh(cmd) return vim.fn.system(cmd) end

local function make_remote()
  local dir = vim.fn.tempname()
  vim.fn.mkdir(dir, "p")
  sh({ "git", "-C", dir, "init", "-q", "-b", "main" })
  sh({ "git", "-C", dir, "config", "user.email", "t@t.t" })
  sh({ "git", "-C", dir, "config", "user.name",  "t"     })
  vim.fn.writefile({ "1" }, dir .. "/init.lua")
  sh({ "git", "-C", dir, "add", "." })
  sh({ "git", "-C", dir, "commit", "-q", "-m", "first" })
  return dir
end

local function fresh()
  package.loaded["core.pack.install"] = nil
  package.loaded["core.pack.lock"] = nil
  package.loaded["core.pack.git"] = nil
  package.loaded["core.pack.jobs"] = nil
  package.loaded["core.pack.version"] = nil
  local I = require("core.pack.install")
  local L = require("core.pack.lock")
  L._path_override = vim.fn.tempname() .. ".json"
  os.remove(L._path_override)
  I._install_root_override = vim.fn.tempname() .. "-pack"
  vim.fn.mkdir(I._install_root_override, "p")
  return I, L
end

T.describe("core.pack.install", function()
  T.it("install_missing skips already-installed plugins", function()
    local I, L = fresh()
    local remote = make_remote()
    local spec = { name = "demo", src = "file://" .. remote }
    -- First install:
    I.install_missing({ spec }, {})
    local rev1 = L.get("demo").rev
    -- Pretend remote moves on; install_missing should not touch the existing checkout.
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    I.install_missing({ spec }, {})
    T.eq(L.get("demo").rev, rev1)
  end)

  T.it("install_missing clones a missing plugin and writes lockfile", function()
    local I, L = fresh()
    local remote = make_remote()
    I.install_missing({ { name = "demo", src = "file://" .. remote } }, {})
    local entry = L.get("demo")
    T.truthy(entry and entry.rev and #entry.rev == 40)
    T.eq(entry.src, "file://" .. remote)
  end)

  T.it("install_missing emits progress events", function()
    local I, L = fresh()
    local r1 = make_remote(); local r2 = make_remote()
    local seen = {}
    I.install_missing(
      { { name = "a", src = "file://" .. r1 }, { name = "b", src = "file://" .. r2 } },
      { on_progress = function(done, total, last) seen[#seen + 1] = { done, total, last } end })
    T.eq(seen[#seen][1], 2)
    T.eq(seen[#seen][2], 2)
  end)

  T.it("install_missing runs string build hook after install", function()
    local I, L = fresh()
    local remote = make_remote()
    local marker = vim.fn.tempname()
    I.install_missing(
      { { name = "demo", src = "file://" .. remote, build = "touch " .. marker } }, {})
    T.truthy(vim.fn.filereadable(marker) == 1)
  end)

  T.it("update fetches new commits and applies when confirm=false", function()
    local I, L = fresh()
    local remote = make_remote()
    I.install_missing({ { name = "demo", src = "file://" .. remote } }, {})
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    local rev_before = L.get("demo").rev
    I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, { confirm = false })
    T.truthy(L.get("demo").rev ~= rev_before)
  end)

  T.it("clean removes plugin not in spec list", function()
    local I, L = fresh()
    local r1 = make_remote(); local r2 = make_remote()
    I.install_missing(
      { { name = "a", src = "file://" .. r1 }, { name = "b", src = "file://" .. r2 } }, {})
    I.clean({ { name = "a", src = "file://" .. r1 } }, {})
    T.eq(L.get("b"), nil)
    T.truthy(L.get("a"))
    T.eq(vim.fn.isdirectory(I.install_dir("b")), 0)
  end)
end)
