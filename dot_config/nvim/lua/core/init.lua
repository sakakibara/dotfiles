local M = {}

function M.setup()
  M.bootstrap()

  M.LazyUtil = require("lazy.core.util")
  local clazy = require("config.lazy")

  require("lazy").setup(clazy)

  local no_argc = vim.fn.argc(-1) == 0
  if not no_argc then
    M.load("autocmds")
  end

  vim.api.nvim_create_autocmd("User", {
    group = vim.api.nvim_create_augroup("Core", { clear = true }),
    pattern = "VeryLazy",
    callback = function()
      if no_argc then
        M.load("autocmds")
      end
      M.load("keymaps")
      require("util.format").setup()
    end,
  })

  M.LazyUtil.track("colorscheme")
  M.LazyUtil.try(function()
    vim.cmd.colorscheme(clazy.install.colorscheme[1])
  end, {
    msg = "Failed to load the colorscheme",
    on_error = function(msg)
      M.LazyUtil.error(msg)
      vim.cmd.colorscheme("colorscheme")
    end,
  })
  M.LazyUtil.track()
end

M.inited = false
function M.init()
  if M.inited then
    return
  end
  M.inited = true
  local uplugin = require("util.plugin")
  uplugin.delay_notify()
  M.load("options")
  uplugin.lazy_file()
end

function M.bootstrap()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable",
      lazypath,
    })
  end
  vim.opt.rtp:prepend(lazypath)
end

function M.load(name)
  local function _load(mod)
    if require("lazy.core.cache").find(mod)[1] then
      M.LazyUtil.try(function()
        require(mod)
      end, { msg = "Failed to load" .. mod })
    end
  end
  _load("config." .. name)
  if vim.bo.filetype == "lazy" then
    vim.cmd([[do VimResized]])
  end
end

return M
