local T = require("tests.helpers")
local function fresh()
  package.loaded["lib.path"] = nil
  return require("lib.path")
end

T.describe("lib.path", function()
  T.it("exposes the platform separator and home as fields", function()
    local P = fresh()
    T.eq(P.sep, package.config:sub(1, 1))
    T.truthy(type(P.home) == "string" and P.home ~= "")
  end)

  T.it("is_remote detects /mnt and UNC-style paths (truthy on match)", function()
    local P = fresh()
    -- is_remote uses string:match, which returns the matched substring (truthy)
    -- or nil — not a strict boolean. Test the contract semantically.
    T.truthy(P.is_remote("/mnt/data/file"))
    T.truthy(P.is_remote("//server/share"))
    T.truthy(P.is_remote("\\\\server\\share"))
    T.eq(P.is_remote("/Users/sho/project") or false, false)
    T.eq(P.is_remote(nil) or false, false)
  end)

  T.it("replace_home swaps the home prefix with ~ but leaves other paths alone", function()
    local P = fresh()
    T.eq(P.replace_home(P.home .. "/foo"), "~/foo")
    T.eq(P.replace_home(P.home), "~")
    T.eq(P.replace_home("/etc/hosts"), "/etc/hosts")
    T.eq(P.replace_home(""), "")
    T.eq(P.replace_home(nil), nil)
  end)

  T.it("split breaks paths on the platform separator and skips empty parts", function()
    local P = fresh()
    T.eq(P.split("/a/b/c"),     { "a", "b", "c" })
    T.eq(P.split("/a//b///c"),  { "a", "b", "c" })  -- collapses runs of separators
    T.eq(P.split(""),           {})
    T.eq(P.split(nil),          {})
  end)

  T.it("short keeps full path when segment count <= max_seg", function()
    local P = fresh()
    T.eq(P.short("/a/b/c", 3), "a" .. P.sep .. "b" .. P.sep .. "c")
    T.eq(P.short("/a/b",   3), "a" .. P.sep .. "b")
  end)

  T.it("short collapses interior segments with an ellipsis when over max_seg", function()
    local P = fresh()
    -- 5 segments, max_seg=3 → keep first + ellipsis + last 2
    local got = P.short("/a/b/c/d/e", 3)
    local want = table.concat({ "a", "…", "d", "e" }, P.sep)
    T.eq(got, want)
  end)

  T.it("short defaults max_seg to 3", function()
    local P = fresh()
    local got = P.short("/a/b/c/d/e")
    local want = table.concat({ "a", "…", "d", "e" }, P.sep)
    T.eq(got, want)
  end)

  T.it("short rewrites home prefix before truncating", function()
    local P = fresh()
    -- Construct a path inside home with enough segments to trigger truncation.
    local long = P.home .. "/" .. table.concat({ "p1", "p2", "p3", "p4" }, "/")
    local got = P.short(long, 2)
    -- After replace_home: ~/p1/p2/p3/p4 → split: {"~","p1","p2","p3","p4"}
    -- Keep first + ellipsis + last 1 (max_seg=2 → kept = parts[1] + "…" + last 1)
    local want = table.concat({ "~", "…", "p4" }, P.sep)
    T.eq(got, want)
  end)

  T.it("exists is true for an existing file and falsey otherwise", function()
    local P = fresh()
    -- Use this very test file as a known-existing path.
    local self_path = debug.getinfo(1, "S").source:gsub("^@", "")
    T.eq(P.exists(self_path), true)
    -- For non-existent paths and nil, exists is falsey (false or nil).
    T.eq(P.exists("/definitely/does/not/exist/" .. tostring(os.time())) or false, false)
    T.eq(P.exists(nil) or false, false)
  end)
end)
