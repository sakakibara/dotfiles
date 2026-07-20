local T = require("tests.helpers")

local function fresh()
  package.loaded["core.pack.history"] = nil
  package.loaded["core.pack.lock"] = nil
  local H = require("core.pack.history")
  local L = require("core.pack.lock")
  L._path_override = vim.fn.tempname() .. ".json"
  H._dir_override = vim.fn.tempname() .. "-hist"
  vim.fn.mkdir(H._dir_override, "p")
  H._max_snapshots = 3
  return H, L
end

T.describe("core.pack.history", function()
  T.it("snapshot copies current lockfile and lists itself", function()
    local H, L = fresh()
    L.set("a", { src = "u/a", rev = "1" })
    H.snapshot()
    local entries = H.list()
    T.eq(#entries, 1)
    T.truthy(entries[1].ts and entries[1].path and entries[1].iso)
  end)

  T.it("snapshot is no-op when lockfile missing", function()
    local H, _ = fresh()
    H.snapshot()  -- nothing to copy; should not error
    T.eq(#H.list(), 0)
  end)

  T.it("rotation drops oldest beyond max_snapshots", function()
    local H, L = fresh()
    for i = 1, 5 do
      L.set("a", { src = "u/a", rev = tostring(i) })
      H.snapshot()
      vim.uv.sleep(15)  -- ensure distinct timestamps
    end
    T.eq(#H.list(), 3)
  end)

  T.it("restore writes snapshot data into the lockfile", function()
    local H, L = fresh()
    L.set("a", { src = "u/a", rev = "1" })
    H.snapshot()
    vim.uv.sleep(15)
    L.set("a", { src = "u/a", rev = "2" })
    local snaps = H.list()
    H.restore(snaps[#snaps].ts)  -- oldest snapshot
    T.eq(L.get("a").rev, "1")
  end)

  T.it("list returns newest first", function()
    local H, L = fresh()
    L.set("a", { src = "u/a", rev = "1" })
    H.snapshot(); vim.uv.sleep(15)
    L.set("a", { src = "u/a", rev = "2" })
    H.snapshot()
    local entries = H.list()
    T.truthy(entries[1].ts > entries[2].ts)
  end)
end)
