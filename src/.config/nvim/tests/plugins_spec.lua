local T = require("tests.helpers")

T.describe("config.plugins", function()
  T.it("reports a spec file that errors and keeps the others", function()
    local root = vim.fn.tempname()
    vim.fn.mkdir(root .. "/lua/config/plugins", "p")
    vim.fn.writefile({ 'error("boom")' }, root .. "/lua/config/plugins/zz_bad.lua")
    vim.fn.writefile({ 'return { { "owner/good" } }' }, root .. "/lua/config/plugins/zz_good.lua")

    local saved_stdpath, saved_notify, saved_rtp = vim.fn.stdpath, vim.notify, vim.o.rtp
    local notes = {}
    vim.fn.stdpath = function(what)
      if what == "config" then return root end
      return saved_stdpath(what)
    end
    vim.notify = function(msg, level) notes[#notes + 1] = { msg = msg, level = level } end
    vim.opt.rtp:prepend(root)

    local ok, specs = pcall(dofile, vim.fn.getcwd() .. "/lua/config/plugins.lua")

    vim.fn.stdpath, vim.notify, vim.o.rtp = saved_stdpath, saved_notify, saved_rtp
    package.loaded["config.plugins.zz_bad"] = nil
    package.loaded["config.plugins.zz_good"] = nil
    vim.fn.delete(root, "rf")

    T.truthy(ok, specs)
    T.eq(specs, { { "owner/good" } })
    T.eq(#notes, 1)
    T.eq(notes[1].level, vim.log.levels.ERROR)
    T.truthy(notes[1].msg:find("config.plugins.zz_bad", 1, true), notes[1].msg)
    T.truthy(notes[1].msg:find("boom", 1, true), notes[1].msg)
  end)
end)
