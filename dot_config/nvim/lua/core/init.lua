local M = {}

function M.setup()
  M.bootstrap()

  if vim.fn.argc(-1) == 0 then
    vim.api.nvim_create_autocmd("User", {
      group = vim.api.nvim_create_augroup("UserConfig", { clear = true }),
      pattern = "VeryLazy",
      callback = function()
        M.load("autocmds")
        M.load("keymaps")
      end,
    })
  else
    M.load("autocmds")
    M.load("keymaps")
  end

  require("lazy").setup(require("config.lazy"))
end

M.inited = false
function M.init()
  if not M.inited then
    M.inited = true
    M.load("options")
  end
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
  local LazyUtil = require("lazy.core.util")
  local function _load(mod)
    LazyUtil.try(function()
      require(mod)
    end, {
      msg = "Failed to load " .. mod,
      on_error = function(msg)
        local info = require("lazy.core.cache").find(mod)
        if info == nil or (type(info) == "table" and #info == 0) then
          return
        end
        LazyUtil.error(msg)
      end,
    })
  end
  _load("config." .. name)
  if vim.bo.filetype == "lazy" then
    vim.cmd([[do VimResized]])
  end
end

return M
