local T = require("tests.helpers")

local function fresh()
  package.loaded["core.pack.log"] = nil
  local L = require("core.pack.log")
  L._path_override = vim.fn.tempname() .. ".jsonl"
  os.remove(L._path_override)
  return L
end

T.describe("core.pack.log", function()
  T.it("append writes a JSON-line entry", function()
    local L = fresh()
    L.append({ ts = 1714293600, kind = "update", name = "demo", from = "abc", to = "def", count = 3 })
    L.append({ ts = 1714293610, kind = "update", name = "other", from = "111", to = "222", count = 1 })
    local entries = L.list()
    T.eq(#entries, 2)
    T.eq(entries[1].name, "other")  -- newest first
    T.eq(entries[2].name, "demo")
  end)

  T.it("list with limit returns at most N", function()
    local L = fresh()
    for i = 1, 5 do
      L.append({ ts = 1714293600 + i, kind = "update", name = "p" .. i, from = "a", to = "b", count = 1 })
    end
    T.eq(#L.list({ limit = 3 }), 3)
  end)

  T.it("list on missing file returns empty table", function()
    local L = fresh()
    T.eq(#L.list(), 0)
  end)
end)
