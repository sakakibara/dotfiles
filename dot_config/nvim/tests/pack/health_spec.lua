local T = require("tests.helpers")
local function fresh() package.loaded["core.pack.health"] = nil; return require("core.pack.health") end

T.describe("core.pack.health", function()
  T.it("reports ok when git is on PATH and lockfile is readable", function()
    local H = fresh()
    package.loaded["core.pack.lock"] = nil
    local L = require("core.pack.lock")
    L._path_override = vim.fn.tempname() .. ".json"
    L.write({ version = 1, plugins = {} })
    local r = H.report()
    -- At least one ok line for "git" and one for "lockfile"
    local has_git = false; local has_lock = false
    for _, item in ipairs(r) do
      if item.kind == "ok" and item.text:lower():match("git") then has_git = true end
      if item.kind == "ok" and item.text:lower():match("lockfile") then has_lock = true end
    end
    T.truthy(has_git)
    T.truthy(has_lock)
  end)

  T.it("warns when lockfile is missing", function()
    local H = fresh()
    package.loaded["core.pack.lock"] = nil
    local L = require("core.pack.lock")
    L._path_override = vim.fn.tempname() .. ".does-not-exist.json"
    local r = H.report()
    local saw_warn = false
    for _, item in ipairs(r) do
      if (item.kind == "warn" or item.kind == "info") and item.text:lower():match("lockfile") then saw_warn = true end
    end
    T.truthy(saw_warn)
  end)

  T.it("errors when install root does not exist", function()
    local H = fresh()
    package.loaded["core.pack.install"] = nil
    local I = require("core.pack.install")
    I._install_root_override = vim.fn.tempname() .. "-nope"
    local r = H.report()
    local saw_err = false
    for _, item in ipairs(r) do
      if item.kind == "warn" and item.text:lower():match("install root") then saw_err = true end
    end
    T.truthy(saw_err)
  end)
end)
