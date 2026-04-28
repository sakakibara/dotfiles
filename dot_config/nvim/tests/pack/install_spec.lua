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
  package.loaded["core.pack.ui"] = nil
  local I = require("core.pack.install")
  local L = require("core.pack.lock")
  L._path_override = vim.fn.tempname() .. ".json"
  os.remove(L._path_override)
  I._install_root_override = vim.fn.tempname() .. "-pack"
  vim.fn.mkdir(I._install_root_override, "p")
  return I, L
end

-- Drive an async function (opts has on_complete) to completion.
local function async(fn)
  local done = false
  fn(function() done = true end)
  vim.wait(60000, function() return done end, 25)
  if not done then error("async: timed out after 60s") end
end

T.describe("core.pack.install", function()
  T.it("install_missing skips already-installed plugins", function()
    local I, L = fresh()
    local remote = make_remote()
    local spec = { name = "demo", src = "file://" .. remote }
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    local rev1 = L.get("demo").rev
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    async(function(cb) I.install_missing({ spec }, { on_complete = cb }) end)
    T.eq(L.get("demo").rev, rev1)
  end)

  T.it("install_missing clones a missing plugin and writes lockfile", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    local entry = L.get("demo")
    T.truthy(entry and entry.rev and #entry.rev == 40)
    T.eq(entry.src, "file://" .. remote)
  end)

  T.it("install_missing emits progress events", function()
    local I, L = fresh()
    local r1 = make_remote(); local r2 = make_remote()
    local seen = {}
    async(function(cb) I.install_missing(
      { { name = "a", src = "file://" .. r1 }, { name = "b", src = "file://" .. r2 } },
      { on_progress = function(done, total, last) seen[#seen + 1] = { done, total, last } end, on_complete = cb })
    end)
    T.eq(seen[#seen][1], 2)
    T.eq(seen[#seen][2], 2)
  end)

  T.it("install_missing runs string build hook after install", function()
    local I, L = fresh()
    local remote = make_remote()
    local marker = vim.fn.tempname()
    async(function(cb) I.install_missing(
      { { name = "demo", src = "file://" .. remote, build = "touch " .. marker } }, { on_complete = cb })
    end)
    T.truthy(vim.fn.filereadable(marker) == 1)
  end)

  T.it("install_missing returns immediately and on_complete fires later", function()
    local I, L = fresh()
    local remote = make_remote()
    local fired = false
    I.install_missing({ { name = "demo", src = "file://" .. remote } }, {
      on_complete = function() fired = true end,
    })
    T.eq(fired, false)
    vim.wait(60000, function() return fired end, 25)
    T.eq(fired, true)
    T.truthy(L.get("demo"))
  end)

  T.it("update fetches new commits and applies when confirm=false", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    local rev_before = L.get("demo").rev
    async(function(cb) I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, { confirm = false, on_complete = cb }) end)
    T.truthy(L.get("demo").rev ~= rev_before)
  end)

  T.it("clean removes plugin not in spec list", function()
    local I, L = fresh()
    local r1 = make_remote(); local r2 = make_remote()
    async(function(cb) I.install_missing(
      { { name = "a", src = "file://" .. r1 }, { name = "b", src = "file://" .. r2 } }, { on_complete = cb })
    end)
    async(function(cb) I.clean({ { name = "a", src = "file://" .. r1 } }, { on_complete = cb }) end)
    T.eq(L.get("b"), nil)
    T.truthy(L.get("a"))
    T.eq(vim.fn.isdirectory(I.install_dir("b")), 0)
  end)

  T.it("install_missing snapshots before writing lockfile", function()
    package.loaded["core.pack.history"] = nil
    local I, L = fresh()
    local H = require("core.pack.history")
    H._dir_override = vim.fn.tempname() .. "-hist"
    vim.fn.mkdir(H._dir_override, "p")
    H._max_snapshots = 5
    local remote = make_remote()
    L.set("seed", { src = "x", rev = "0" })
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    T.truthy(#H.list() >= 1)
  end)

  T.it("update with target=lockfile checks out lockfile rev (rollback flow)", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    local rev_a = L.get("demo").rev
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })
    async(function(cb) I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, { confirm = false, on_complete = cb }) end)
    local rev_b = L.get("demo").rev
    T.truthy(rev_a ~= rev_b, "remote update should advance lockfile")
    L.set("demo", { src = "file://" .. remote, rev = rev_a })
    async(function(cb) I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, { confirm = false, target = "lockfile", on_complete = cb }) end)
    local Git = require("core.pack.git")
    T.eq(Git.current_rev(I.install_dir("demo")), rev_a)
  end)

  T.it("update with confirm=true fires on_complete only after lockfile advances", function()
    local I, L = fresh()
    local remote = make_remote()
    async(function(cb) I.install_missing({ { name = "demo", src = "file://" .. remote } }, { on_complete = cb }) end)
    local rev_a = L.get("demo").rev
    sh({ "git", "-C", remote, "commit", "--allow-empty", "-q", "-m", "second" })

    -- Stub UI.update_review to drive on_apply then on_close synchronously, the same way
    -- the <CR> keymap does. This reproduces the race that broke before the fix.
    -- fresh() already loaded core.pack.ui; grab that same table so install.lua's
    -- local UI upvalue sees the stub.
    local UI = require("core.pack.ui")
    UI.update_review = function(items, opts)
      opts.on_apply(items)  -- apply all
      opts.on_close()       -- mimic <CR> keymap's apply+close sequence
      return { close = function() end }
    end

    local complete_called_at_rev
    I.update({ { name = "demo", src = "file://" .. remote } }, { "demo" }, {
      confirm = true, open_window = false,
      on_complete = function()
        complete_called_at_rev = L.get("demo").rev
      end,
    })
    vim.wait(60000, function() return complete_called_at_rev ~= nil end, 25)
    T.truthy(complete_called_at_rev, "on_complete must fire")
    T.truthy(complete_called_at_rev ~= rev_a, "on_complete must fire AFTER lockfile advances")
  end)
end)
