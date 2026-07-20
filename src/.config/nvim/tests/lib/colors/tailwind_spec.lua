local T = require("tests.helpers")
local TW = require("lib.colors.tailwind")

T.describe("lib.colors.tailwind palette lookup", function()
  T.it("resolves red-500 to a Color", function()
    local c = TW.resolve("red-500")
    T.truthy(c, "resolve returned nil")
    T.truthy(c.source.fmt == "oklch")
    -- Tailwind v4 red-500 ≈ oklch(0.637 0.237 25.331); should be red-ish
    T.truthy(c.r > 0.7 and c.r < 1.0, "expected red-ish r, got " .. c.r)
  end)

  T.it("resolves blue-500", function()
    local c = TW.resolve("blue-500")
    T.truthy(c)
    T.truthy(c.b > 0.6, "expected blue-ish b, got " .. c.b)
  end)

  T.it("returns nil for unknown classes", function()
    T.eq(TW.resolve("not-a-class-500"), nil)
  end)

  T.it("strips utility prefix (bg-/text-/border-/...)", function()
    local c = TW.resolve_class("bg-red-500")
    T.truthy(c)
    local c2 = TW.resolve_class("text-blue-500")
    T.truthy(c2)
  end)
end)

T.describe("lib.colors.tailwind project @theme scan", function()
  T.it("scan_file harvests --color-* declarations from @theme", function()
    TW._overlay = {}
    local fixture = vim.fn.getcwd() .. "/tests/lib/colors/fixtures/theme.css"
    local done = false
    TW.scan_file(fixture, function() done = true end)
    vim.wait(2000, function() return done end)
    T.truthy(done, "scan_file callback never fired")
    T.truthy(TW._overlay["brand-500"], "brand-500 not in overlay")
    T.truthy(TW._overlay["warn"], "warn not in overlay")
  end)

  T.it("resolve uses overlay before built-in", function()
    TW._overlay = { ["red-500"] = { 0.5, 0.5, 0 } }
    local c = TW.resolve("red-500")
    T.truthy(c, "resolve returned nil")
    -- Reset for downstream tests
    TW._overlay = {}
  end)
end)

T.describe("lib.colors.tailwind async walk", function()
  T.it("follows symlinks and terminates on cycles", function()
    local tmp = vim.fn.tempname()
    vim.fn.mkdir(tmp .. "/real", "p")
    vim.fn.mkdir(tmp .. "/target", "p")
    -- A css file reachable ONLY by following a symlink.
    local f = io.open(tmp .. "/target/themed.css", "w")
    f:write("@theme { --color-z: #0000ff; }\n"); f:close()
    vim.uv.fs_symlink(tmp .. "/target", tmp .. "/link")
    -- A symlink cycle: real/loop points back at an ancestor.
    vim.uv.fs_symlink(tmp, tmp .. "/real/loop")

    local done, files = false, nil
    TW._walk_css_async(tmp, function(out) files = out; done = true end)
    -- Bounds the cycle: if the guard regresses, pending never drains, this
    -- times out and the test FAILS rather than hanging the runner.
    local ok = vim.wait(3000, function() return done end, 10)
    T.truthy(ok, "walk did not terminate within 3s (symlink-cycle guard failed)")

    local found = false
    for _, p in ipairs(files or {}) do
      if p:match("themed%.css$") then found = true end
    end
    T.truthy(found, "css reached via symlink was not found")

    vim.fn.delete(tmp, "rf")
  end)
end)

T.describe("lib.colors.tailwind overlay file-scope tracking", function()
  T.it("scan_file removes stale entries on re-scan", function()
    TW._overlay = {}
    TW._overlay_by_file = {}
    local fixture = vim.fn.tempname() .. ".css"

    -- Initial: write file with two declarations, scan
    local f = io.open(fixture, "w")
    f:write("@theme {\n  --color-x: #ff0000;\n  --color-y: #00ff00;\n}\n")
    f:close()
    local done = false
    TW.scan_file(fixture, function() done = true end)
    vim.wait(2000, function() return done end)
    T.truthy(TW._overlay["x"], "x should be in overlay")
    T.truthy(TW._overlay["y"], "y should be in overlay")

    -- Rewrite without `y` and re-scan; `y` should be removed from overlay
    f = io.open(fixture, "w")
    f:write("@theme {\n  --color-x: #ff0000;\n}\n")
    f:close()
    done = false
    TW.scan_file(fixture, function() done = true end)
    vim.wait(2000, function() return done end)
    T.truthy(TW._overlay["x"], "x should still be in overlay")
    T.eq(TW._overlay["y"], nil, "y should be removed from overlay")

    os.remove(fixture)
  end)
end)
