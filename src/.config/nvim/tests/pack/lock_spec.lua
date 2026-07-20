local T = require("tests.helpers")

local function fresh_lock()
  package.loaded["core.pack.lock"] = nil
  local L = require("core.pack.lock")
  -- Redirect to a tmp path so tests don't touch the real file.
  L._path_override = vim.fn.tempname() .. ".json"
  os.remove(L._path_override)
  return L
end

T.describe("core.pack.lock", function()
  T.it("read() on missing file returns empty table shape", function()
    local L = fresh_lock()
    T.eq(L.read(), { version = 1, plugins = {} })
  end)

  T.it("write() then read() roundtrips", function()
    local L = fresh_lock()
    L.write({ version = 1, plugins = { ["a.nvim"] = { src = "x", rev = "abc" } } })
    T.eq(L.read().plugins["a.nvim"].rev, "abc")
  end)

  T.it("set() upserts a single plugin without rewriting others", function()
    local L = fresh_lock()
    L.set("a", { src = "u/a", rev = "111" })
    L.set("b", { src = "u/b", rev = "222" })
    L.set("a", { src = "u/a", rev = "333" })
    local data = L.read()
    T.eq(data.plugins.a.rev, "333")
    T.eq(data.plugins.b.rev, "222")
  end)

  T.it("delete() removes a plugin", function()
    local L = fresh_lock()
    L.set("a", { src = "u/a", rev = "1" })
    L.delete("a")
    T.eq(L.read().plugins.a, nil)
  end)

  T.it("migrate_from_vim_pack copies entries when target missing", function()
    local L = fresh_lock()
    local src_path = vim.fn.tempname() .. "-vimpack.json"
    local f = io.open(src_path, "w")
    f:write('{"plugins":{"x":{"src":"u/x","rev":"deadbeef"}}}')
    f:close()
    L._vim_pack_path_override = src_path
    local migrated = L.migrate_from_vim_pack()
    T.truthy(migrated)
    T.eq(L.read().plugins.x.rev, "deadbeef")
  end)

  T.it("migrate_from_vim_pack is a no-op when target already populated", function()
    local L = fresh_lock()
    L.set("existing", { src = "u/e", rev = "1" })
    local src_path = vim.fn.tempname() .. "-vimpack.json"
    local f = io.open(src_path, "w")
    f:write('{"plugins":{"x":{"src":"u/x","rev":"deadbeef"}}}')
    f:close()
    L._vim_pack_path_override = src_path
    local migrated = L.migrate_from_vim_pack()
    T.eq(migrated, false)
    T.eq(L.read().plugins.x, nil)
  end)
end)
