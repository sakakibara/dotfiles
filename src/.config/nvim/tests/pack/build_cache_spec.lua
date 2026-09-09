local T = require("tests.helpers")
local stubs = require("tests.pack.stubs")

local function sh(cmd) return vim.fn.system(cmd) end

local function make_repo()
  local dir = vim.fn.tempname()
  vim.fn.mkdir(dir, "p")
  sh({ "git", "-C", dir, "init", "-q", "-b", "main" })
  sh({ "git", "-C", dir, "config", "user.email", "t@t.t" })
  sh({ "git", "-C", dir, "config", "user.name", "t" })
  vim.fn.writefile({ "1" }, dir .. "/f.txt")
  sh({ "git", "-C", dir, "add", "f.txt" })
  sh({ "git", "-C", dir, "commit", "-q", "-m", "first" })
  return dir
end

local function commit(dir, body)
  vim.fn.writefile({ body }, dir .. "/f.txt")
  sh({ "git", "-C", dir, "commit", "-q", "-am", body })
end

local function fresh()
  package.loaded["core.pack.build_cache"] = nil
  return require("core.pack.build_cache")
end

T.describe("core.pack.build_cache", function()
  T.it("treats a plugin with no build hook as always fresh", function()
    local B, dir = fresh(), make_repo()
    T.eq(stubs.await(B.is_fresh, dir, nil), true)
    T.eq(stubs.await(B.is_fresh, dir, ""), true)
  end)

  T.it("is stale until the build is marked, then fresh", function()
    local B, dir = fresh(), make_repo()
    T.eq(stubs.await(B.is_fresh, dir, "make"), false)
    stubs.await(B.mark_built, dir, "make")
    T.eq(stubs.await(B.is_fresh, dir, "make"), true)
  end)

  T.it("goes stale again when HEAD moves", function()
    local B, dir = fresh(), make_repo()
    stubs.await(B.mark_built, dir, "make")
    commit(dir, "2")
    T.eq(stubs.await(B.is_fresh, dir, "make"), false)
  end)

  T.it("keys the cache on the build command", function()
    local B, dir = fresh(), make_repo()
    stubs.await(B.mark_built, dir, "make")
    T.eq(stubs.await(B.is_fresh, dir, "make"), true)
    T.eq(stubs.await(B.is_fresh, dir, "cargo build"), false)
  end)

  T.it("keys a function build on where it is defined", function()
    local B, dir = fresh(), make_repo()
    local build = function() end
    stubs.await(B.mark_built, dir, build)
    T.eq(stubs.await(B.is_fresh, dir, build), true)
    T.eq(stubs.await(B.is_fresh, dir, function() end), false)
  end)

  T.it("invalidate drops every cached entry for the plugin", function()
    local B, dir = fresh(), make_repo()
    stubs.await(B.mark_built, dir, "make")
    stubs.await(B.mark_built, dir, "cargo build")
    stubs.await(B.invalidate, dir)
    T.eq(stubs.await(B.is_fresh, dir, "make"), false)
    T.eq(stubs.await(B.is_fresh, dir, "cargo build"), false)
  end)

  T.it("keeps its refs out of the branch and tag namespaces", function()
    local B, dir = fresh(), make_repo()
    stubs.await(B.mark_built, dir, "make")
    T.eq(vim.trim(sh({ "git", "-C", dir, "tag", "-l" })), "")
    T.eq(vim.trim(sh({ "git", "-C", dir, "for-each-ref", "--format=%(refname)", "refs/heads/" })), "refs/heads/main")
  end)

  T.it("reports stale for a directory that is not a repository", function()
    local B = fresh()
    local dir = vim.fn.tempname()
    vim.fn.mkdir(dir, "p")
    T.eq(stubs.await(B.is_fresh, dir, "make"), false)
  end)
end)
