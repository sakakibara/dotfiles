local T = require("tests.helpers")
local function fresh()
  package.loaded["lib.path"] = nil
  return require("lib.path")
end

T.describe("lib.path", function()
  T.it("exposes home as a field", function()
    local P = fresh()
    T.truthy(type(P.home) == "string" and P.home ~= "")
  end)

  T.it("replace_home swaps the home prefix with ~ but leaves other paths alone", function()
    local P = fresh()
    T.eq(P.replace_home(P.home .. "/foo"), "~/foo")
    T.eq(P.replace_home(P.home), "~")
    T.eq(P.replace_home("/etc/hosts"), "/etc/hosts")
    T.eq(P.replace_home(""), "")
    T.eq(P.replace_home(nil), nil)
  end)

  T.it("replace_home only replaces home at a path boundary", function()
    local P = fresh()
    T.eq(P.replace_home(P.home .. "ko/x"), P.home .. "ko/x")
    T.eq(P.replace_home(P.home .. "ko"), P.home .. "ko")
  end)
end)

T.describe("lib.path.buf_get_name", function()
  T.it("returns an ordinary buffer's name unchanged", function()
    local P = fresh()
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, "/tmp/plain.txt")
    T.eq(P.buf_get_name(buf), vim.api.nvim_buf_get_name(buf))
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("resolves an oil buffer to its directory by filetype when the name is not a URI", function()
    local P = fresh()
    package.loaded["oil"] = { get_current_dir = function() return "/srv/by-ft" end }
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, "/tmp/plain-name")
    vim.bo[buf].filetype = "oil"
    T.eq(P.buf_get_name(buf), "/srv/by-ft")
    vim.api.nvim_buf_delete(buf, { force = true })
    package.loaded["oil"] = nil
  end)

  T.it("keeps the buffer name when oil is present without get_current_dir", function()
    local P = fresh()
    package.loaded["oil"] = {}
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, "oil:///srv/plain")
    T.eq(P.buf_get_name(buf), "oil:///srv/plain")
    package.loaded["oil"] = nil
  end)

  T.it("resolves an oil buffer to its directory, by URI before the filetype is set", function()
    local P = fresh()
    package.loaded["oil"] = { get_current_dir = function() return "/srv/dir" end }
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, "oil:///srv/dir")
    T.eq(P.buf_get_name(buf), "/srv/dir")
    vim.api.nvim_buf_delete(buf, { force = true })
    package.loaded["oil"] = nil
  end)

  T.it("falls back to an empty name, which renders as no name, not a split URI", function()
    local P = fresh()
    package.loaded["oil"] = { get_current_dir = function() return nil end }
    local buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_buf_set_name(buf, "oil:///srv/other")
    vim.bo[buf].filetype = "oil"
    T.eq(P.buf_get_name(buf), "")
    vim.api.nvim_buf_delete(buf, { force = true })
    package.loaded["oil"] = nil
  end)
end)
