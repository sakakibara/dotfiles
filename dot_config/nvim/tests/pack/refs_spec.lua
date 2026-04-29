-- tests/pack/refs_spec.lua
local T = require("tests.helpers")
local stubs = require("tests.pack.stubs")

local function reset_refs()
  package.loaded["core.pack.refs"] = nil
  package.loaded["core.pack.git"] = nil
  package.loaded["core.pack.version"] = nil
  return require("core.pack.refs")
end

T.describe("core.pack.refs.validate", function()
  T.it("accepts a plain branch name", function()
    T.eq(reset_refs().validate("master"), true)
  end)
  T.it("accepts HEAD", function()
    T.eq(reset_refs().validate("HEAD"), true)
  end)
  T.it("accepts a SHA", function()
    T.eq(reset_refs().validate("abc1234567890def"), true)
  end)
  T.it("rejects empty", function()
    local ok = reset_refs().validate("")
    T.eq(ok, false)
  end)
  T.it("rejects leading dash", function()
    local ok = reset_refs().validate("-bmaster")
    T.eq(ok, false)
  end)
  T.it("rejects --orphan", function()
    local ok = reset_refs().validate("--orphan")
    T.eq(ok, false)
  end)
end)

T.describe("core.pack.refs.qualified", function()
  T.it("tag → refs/tags/<name>", function()
    T.eq(reset_refs().qualified("tag", "v1.0"), "refs/tags/v1.0")
  end)
  T.it("branch → refs/heads/<name>", function()
    T.eq(reset_refs().qualified("branch", "main"), "refs/heads/main")
  end)
  T.it("commit → name unchanged", function()
    T.eq(reset_refs().qualified("commit", "abc123"), "abc123")
  end)
  T.it("default → refs/remotes/origin/HEAD", function()
    T.eq(reset_refs().qualified("default", nil), "refs/remotes/origin/HEAD")
  end)
end)

T.describe("core.pack.refs.resolve", function()
  T.it("nil version → default branch SHA", function()
    local Refs = reset_refs()
    local restore = stubs.stub_system({
      ["tag --list"] = { code = 0, stdout = "" },
      ["branch -r --format=%(refname:short)"] = { code = 0, stdout = "origin/main\n" },
      ["symbolic-ref refs/remotes/origin/HEAD"] = { code = 0, stdout = "refs/remotes/origin/main\n" },
      ["rev-parse --verify refs/remotes/origin/HEAD^{commit}"] = { code = 0, stdout = "deadbeef1234567\n" },
    })
    local resolved, err = Refs.resolve({ name = "p" }, "/some/dir")
    restore()
    T.eq(err, nil)
    T.truthy(resolved, "expected resolved table")
    T.eq(resolved.kind, "default")
    T.eq(resolved.sha, "deadbeef1234567")
  end)

  T.it("explicit tag version → tag SHA", function()
    local Refs = reset_refs()
    local restore = stubs.stub_system({
      ["tag --list"] = { code = 0, stdout = "v1.0\nv1.1\n" },
      ["branch -r --format=%(refname:short)"] = { code = 0, stdout = "origin/main\n" },
      ["symbolic-ref refs/remotes/origin/HEAD"] = { code = 0, stdout = "refs/remotes/origin/main\n" },
      ["rev-parse --verify refs/tags/v1.1^{commit}"] = { code = 0, stdout = "aabbcc111222333\n" },
    })
    local resolved = Refs.resolve({ name = "p", version = "v1.1" }, "/some/dir")
    restore()
    T.eq(resolved.kind, "tag")
    T.eq(resolved.name, "v1.1")
    T.eq(resolved.sha, "aabbcc111222333")
  end)

  T.it("rev-parse failure returns named error", function()
    local Refs = reset_refs()
    local restore = stubs.stub_system({
      ["tag --list"] = { code = 0, stdout = "" },
      ["branch -r --format=%(refname:short)"] = { code = 0, stdout = "origin/main\n" },
      ["symbolic-ref refs/remotes/origin/HEAD"] = { code = 0, stdout = "refs/remotes/origin/main\n" },
      ["rev-parse --verify refs/remotes/origin/HEAD^{commit}"] = { code = 1, stderr = "bad ref\n" },
    })
    local resolved, err = Refs.resolve({ name = "p" }, "/some/dir")
    restore()
    T.eq(resolved, nil)
    T.truthy(err and err:match("did not resolve"))
  end)
end)
