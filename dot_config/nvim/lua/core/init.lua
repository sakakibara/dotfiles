local M = {}

function M.setup()
  M.bootstrap()

  local no_argc = vim.fn.argc(-1) == 0
  if not no_argc then M.load("autocmds") end

  vim.api.nvim_create_autocmd("User", {
    group = vim.api.nvim_create_augroup("Core", { clear = true }),
    pattern = "VeryLazy",
    callback = function()
      if no_argc then M.load("autocmds") end
      M.load("keymaps")
    end,
  })

  M.lazy_file()

  require("lazy").setup(require("config.lazy"))
end

M.inited = false
function M.init()
  if not M.inited then
    M.inited = true
    require("util.lazy").delay_notify()
    M.load("options")
    local Event = require("lazy.core.handler.event")
    local _event = Event._event
    ---@diagnostic disable-next-line: duplicate-set-field
    Event._event = function(self, value)
      return value == "LazyFile" and "User LazyFile" or _event(self, value)
    end
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
    LazyUtil.try(
      function()
        require(mod)
      end,
      {
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

function M.lazy_file()
  local events = {}
  local function load()
    if #events == 0 then
      return
    end
    vim.api.nvim_del_augroup_by_name("lazy_file")
    vim.api.nvim_exec_autocmds("User", { pattern = "LazyFile", modeline = false })
    for _, event in ipairs(events) do
      vim.api.nvim_exec_autocmds(event.event, {
        pattern = event.pattern,
        modeline = false,
        buffer = event.buf,
        data = { lazy_file = true },
      })
    end
    vim.api.nvim_exec_autocmds("CursorMoved", { modeline = false })
    events = {}
  end

  load = vim.schedule_wrap(load)

  vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile", "BufWritePost" }, {
    group = vim.api.nvim_create_augroup("lazy_file", { clear = true }),
    callback = function(event)
      table.insert(events, event)
      load()
    end,
  })
end

return M
