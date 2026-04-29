-- tests/pack/spec_spec.lua
local T = require("tests.helpers")
local F = require("tests.pack.fixtures")

local function reset_spec()
  package.loaded["core.pack.spec"] = nil
  return require("core.pack.spec")
end

T.describe("core.pack.spec._merge: collections", function()
  T.it("dependencies: union, dedup", function()
    local Spec = reset_spec()
    local a = Spec.normalize(F.with_deps("p", { "x", "y" }))
    local b = Spec.normalize(F.with_deps("p", { "y", "z" }))
    local merged = Spec._merge(a, b)
    table.sort(merged.dependencies)
    T.eq(merged.dependencies, { "x", "y", "z" })
  end)

  T.it("opts: deep merge with later overriding nested values", function()
    local Spec = reset_spec()
    local a = Spec.normalize(F.with_opts("p", { ui = { width = 10, height = 20 }, debug = false }))
    local b = Spec.normalize(F.with_opts("p", { ui = { width = 30 }, extra = true }))
    local merged = Spec._merge(a, b)
    T.eq(merged.opts.ui.width, 30)
    T.eq(merged.opts.ui.height, 20)
    T.eq(merged.opts.debug, false)
    T.eq(merged.opts.extra, true)
  end)

  T.it("keys: union by lhs+mode, later additions appended", function()
    local Spec = reset_spec()
    local k1 = { "<F60>", function() end, mode = "n", desc = "a" }
    local k2 = { "<F61>", function() end, mode = "n", desc = "b" }
    local a = Spec.normalize({ dev = true, name = "p", keys = { k1 } })
    local b = Spec.normalize({ dev = true, name = "p", keys = { k2 } })
    local merged = Spec._merge(a, b)
    T.eq(#merged.keys, 2)
  end)

  T.it("event: union of strings", function()
    local Spec = reset_spec()
    local a = Spec.normalize({ dev = true, name = "p", event = "BufRead" })
    local b = Spec.normalize({ dev = true, name = "p", event = { "BufWrite", "BufRead" } })
    local merged = Spec._merge(a, b)
    table.sort(merged.event)
    T.eq(merged.event, { "BufRead", "BufWrite" })
  end)

  T.it("ft: union of strings", function()
    local Spec = reset_spec()
    local a = Spec.normalize({ dev = true, name = "p", ft = "lua" })
    local b = Spec.normalize({ dev = true, name = "p", ft = { "lua", "vim" } })
    local merged = Spec._merge(a, b)
    table.sort(merged.ft)
    T.eq(merged.ft, { "lua", "vim" })
  end)

  T.it("cmd: union of strings", function()
    local Spec = reset_spec()
    local a = Spec.normalize({ dev = true, name = "p", cmd = "Foo" })
    local b = Spec.normalize({ dev = true, name = "p", cmd = { "Bar", "Foo" } })
    local merged = Spec._merge(a, b)
    table.sort(merged.cmd)
    T.eq(merged.cmd, { "Bar", "Foo" })
  end)
end)

T.describe("core.pack.spec._merge: boolean-style fields", function()
  T.it("priority: max", function()
    local Spec = reset_spec()
    local a = Spec.normalize({ dev = true, name = "p", priority = 50 })
    local b = Spec.normalize({ dev = true, name = "p", priority = 100 })
    T.eq(Spec._merge(a, b).priority, 100)
    T.eq(Spec._merge(b, a).priority, 100)  -- order-independent
  end)

  T.it("lazy: any non-lazy → eager", function()
    local Spec = reset_spec()
    local a = Spec.normalize({ dev = true, name = "p", lazy = true })
    local b = Spec.normalize({ dev = true, name = "p", lazy = false })
    T.eq(Spec._merge(a, b).lazy, false)
    T.eq(Spec._merge(b, a).lazy, false)
  end)

  T.it("enabled: any disabled → disabled", function()
    local Spec = reset_spec()
    local a = Spec.normalize({ dev = true, name = "p", enabled = true })
    local b = Spec.normalize({ dev = true, name = "p", enabled = false })
    T.eq(Spec._merge(a, b).enabled, false)
    T.eq(Spec._merge(b, a).enabled, false)
  end)

  T.it("dev: any dev → dev", function()
    local Spec = reset_spec()
    local a = Spec.normalize({ dev = false, src = "https://x/y", name = "p" })
    local b = Spec.normalize({ dev = true, name = "p" })
    T.eq(Spec._merge(a, b).dev, true)
    T.eq(Spec._merge(b, a).dev, true)
  end)
end)

T.describe("core.pack.spec._merge: scalar conflicts", function()
  T.it("conflicting version emits warning, last-wins", function()
    local Spec = reset_spec()
    local a = Spec.normalize(F.with_version("p", "1.*"))
    local b = Spec.normalize(F.with_version("p", "2.*"))
    local merged, warnings = Spec._merge(a, b)
    T.eq(merged.version, b.version)
    T.eq(#warnings, 1)
    T.truthy(warnings[1]:match("p:"), "expected spec name in warning")
    T.truthy(warnings[1]:match("conflicting version"), "expected 'conflicting version' in warning")
  end)

  T.it("identical scalars: silent (no warning)", function()
    local Spec = reset_spec()
    local a = Spec.normalize(F.with_version("p", "1.*"))
    local b = Spec.normalize(F.with_version("p", "1.*"))
    local _, warnings = Spec._merge(a, b)
    T.eq(#warnings, 0)
  end)

  T.it("conflicting branch emits warning", function()
    local Spec = reset_spec()
    local a = Spec.normalize({ dev = true, name = "p", branch = "main" })
    local b = Spec.normalize({ dev = true, name = "p", branch = "develop" })
    local merged, warnings = Spec._merge(a, b)
    T.eq(merged.branch, "develop")
    T.eq(#warnings, 1)
  end)

  T.it("function field conflict (both non-nil) warns", function()
    local Spec = reset_spec()
    local fn1 = function() return 1 end
    local fn2 = function() return 2 end
    local a = Spec.normalize({ dev = true, name = "p", config = fn1 })
    local b = Spec.normalize({ dev = true, name = "p", config = fn2 })
    local merged, warnings = Spec._merge(a, b)
    T.eq(merged.config, fn2)
    T.eq(#warnings, 1)
    T.truthy(warnings[1]:match("conflicting config"))
  end)

  T.it("function field with one nil: silent", function()
    local Spec = reset_spec()
    local fn = function() return 1 end
    local a = Spec.normalize({ dev = true, name = "p", config = fn })
    local b = Spec.normalize({ dev = true, name = "p" })
    local merged, warnings = Spec._merge(a, b)
    T.eq(merged.config, fn)
    T.eq(#warnings, 0)
  end)

  T.it("nil-safe scalar merge: nil from b doesn't override a's value", function()
    local Spec = reset_spec()
    local a = Spec.normalize(F.with_version("p", "1.*"))
    local b = Spec.normalize({ dev = true, name = "p" })  -- no version
    local merged, _ = Spec._merge(a, b)
    T.truthy(merged.version, "expected a's version to survive nil from b")
  end)

  T.it("branch-derived version: branch conflict produces only ONE warning", function()
    local Spec = reset_spec()
    -- Both specs have version derived from branch (normalize fallback).
    -- The branch fields differ, which means version (== branch on each
    -- side) also differs. Without the derived_from_branch guard we'd
    -- produce two warnings; with it, we produce one (branch only).
    local a = Spec.normalize({ dev = true, name = "p", branch = "main" })
    local b = Spec.normalize({ dev = true, name = "p", branch = "develop" })
    local _, warnings = Spec._merge(a, b)
    T.eq(#warnings, 1, "expected exactly one warning (branch-only); got " .. #warnings)
    T.truthy(warnings[1]:match("conflicting branch"))
  end)
end)

T.describe("core.pack.spec.resolve", function()
  T.it("dedup identical entries: no warning, single canonical spec", function()
    local Spec = reset_spec()
    local raw = { F.minimal_lazy("p"), F.minimal_lazy("p") }
    local specs, warnings = Spec.resolve(raw)
    T.eq(#specs, 1)
    T.eq(specs[1].name, "p")
    T.eq(#warnings, 0)
  end)

  T.it("preserves first-occurrence insertion order", function()
    local Spec = reset_spec()
    local raw = {
      F.minimal_eager("a"),
      F.minimal_eager("b"),
      F.minimal_eager("a"),  -- duplicate, doesn't shift order
      F.minimal_eager("c"),
    }
    local specs = Spec.resolve(raw)
    T.eq(#specs, 3)
    T.eq(specs[1].name, "a")
    T.eq(specs[2].name, "b")
    T.eq(specs[3].name, "c")
  end)

  T.it("three-way merge: collections accumulate", function()
    local Spec = reset_spec()
    local raw = {
      F.with_deps("p", { "x" }),
      F.with_deps("p", { "y" }),
      F.with_deps("p", { "z" }),
    }
    local specs = Spec.resolve(raw)
    table.sort(specs[1].dependencies)
    T.eq(specs[1].dependencies, { "x", "y", "z" })
  end)

  T.it("conflict warning bubbles up to resolve", function()
    local Spec = reset_spec()
    local raw = {
      F.with_version("p", "1.*"),
      F.with_version("p", "2.*"),
    }
    local _, warnings = Spec.resolve(raw)
    T.eq(#warnings, 1)
    T.truthy(warnings[1]:match("conflicting version"))
  end)
end)
