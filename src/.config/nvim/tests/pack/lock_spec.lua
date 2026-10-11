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

  local function corrupt(L)
    local raw = '{ "plugins": { "a": { "rev": "1" }, "b": '
    local fd = assert(io.open(L.path(), "w")); fd:write(raw); fd:close()
    return raw
  end

  local function slurp(p)
    local fd = assert(io.open(p, "r")); local s = fd:read("*a"); fd:close(); return s
  end

  local function quiet(fn)
    local msgs = {}
    local orig = vim.notify
    vim.notify = function(m, l) msgs[#msgs + 1] = { m, l } end
    local ok, err = pcall(fn)
    vim.notify = orig
    if not ok then error(err) end
    return msgs
  end

  T.it("read() on corrupt JSON returns nil + error and notifies", function()
    local L = fresh_lock()
    corrupt(L)
    local data, err
    local msgs = quiet(function() data, err = L.read() end)
    T.eq(data, nil)
    T.truthy(err and err:match("unreadable"))
    T.eq(#msgs, 1)
    T.eq(msgs[1][2], vim.log.levels.ERROR)
  end)

  T.it("set() and delete() refuse to overwrite a corrupt lockfile", function()
    local L = fresh_lock()
    local raw = corrupt(L)
    local ok_set, ok_del
    quiet(function()
      ok_set = L.set("c", { src = "u/c", rev = "3" })
      ok_del = L.delete("a")
    end)
    T.eq(ok_set, false)
    T.eq(ok_del, false)
    T.eq(slurp(L.path()), raw)
  end)

  T.it("a batch over a corrupt lockfile never commits", function()
    local L = fresh_lock()
    local raw = corrupt(L)
    quiet(function()
      L.batch_begin()
      L.set("c", { src = "u/c", rev = "3" })
      L.batch_commit()
    end)
    T.eq(L._batch, nil)
    T.eq(slurp(L.path()), raw)
  end)
end)
