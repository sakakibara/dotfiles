local T = require("tests.helpers")

local function ts_spec()
  package.loaded["config.plugins.treesitter"] = nil
  for _, s in ipairs(require("config.plugins.treesitter")) do
    if s[1] == "nvim-treesitter/nvim-treesitter" then return s end
  end
end

T.describe("config.plugins.treesitter", function()
  T.it("keeps the config's markdown folds query ahead of the parser install dir", function()
    local data = vim.fn.tempname()
    vim.fn.mkdir(data .. "/site/queries/markdown", "p")
    vim.fn.writefile({ "(section) @fold" }, data .. "/site/queries/markdown/folds.scm")

    local saved_rtp = vim.o.rtp
    local saved_stdpath = vim.fn.stdpath
    local mods = { "nvim-treesitter", "nvim-treesitter.log", "nvim-treesitter.config" }
    local saved = {}
    for _, m in ipairs(mods) do saved[m] = package.loaded[m] end

    package.loaded["nvim-treesitter"] = {
      setup = function(opts)
        if opts and opts.install_dir then vim.o.rtp = opts.install_dir .. "," .. vim.o.rtp end
      end,
      install = function() end,
    }
    package.loaded["nvim-treesitter.log"] = { Logger = {} }
    package.loaded["nvim-treesitter.config"] = { get_installed = function() return {} end }
    vim.fn.stdpath = function(what)
      if what == "data" then return data end
      return saved_stdpath(what)
    end

    local ok, err = pcall(function()
      ts_spec().config()
      local files = vim.treesitter.query.get_files("markdown", "folds")
      T.eq(files[1], vim.fn.getcwd() .. "/queries/markdown/folds.scm")
    end)

    vim.fn.stdpath = saved_stdpath
    vim.o.rtp = saved_rtp
    for _, m in ipairs(mods) do package.loaded[m] = saved[m] end
    pcall(vim.api.nvim_del_augroup_by_name, "Lib.treesitter.highlight")
    pcall(vim.api.nvim_del_augroup_by_name, "Lib.treesitter.org_src")
    vim.fn.delete(data, "rf")
    if not ok then error(err) end
  end)
end)
