local T = require("tests.helpers")
local function fresh() package.loaded["core.pack.version"] = nil; return require("core.pack.version") end

local refs = {
  tags = { "v1.0.0", "v1.1.0", "v1.2.3", "v2.0.0" },
  branches = { "main", "develop" },
  default_branch = "main",
}

T.describe("core.pack.version.resolve", function()
  T.it("nil version → default sentinel", function()
    local V = fresh()
    local r = V.resolve(nil, refs)
    T.eq(r.kind, "default")
    T.eq(r.name, nil)
  end)

  T.it("exact tag string → tag", function()
    local V = fresh()
    T.eq(V.resolve("v1.0.0", refs), { kind = "tag", name = "v1.0.0" })
  end)

  T.it("branch string → branch (matched against branches list)", function()
    local V = fresh()
    T.eq(V.resolve("develop", refs), { kind = "branch", name = "develop" })
  end)

  T.it("40-char hex string → commit", function()
    local V = fresh()
    local sha = string.rep("a", 40)
    T.eq(V.resolve(sha, refs), { kind = "commit", name = sha })
  end)

  T.it("vim.version range picks highest matching tag", function()
    local V = fresh()
    local range = vim.version.range("1")
    T.eq(V.resolve(range, refs), { kind = "tag", name = "v1.2.3" })
  end)

  T.it("vim.version range with no match returns nil", function()
    local V = fresh()
    local range = vim.version.range("9.0")
    T.eq(V.resolve(range, refs), nil)
  end)
end)
