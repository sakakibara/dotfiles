local M = {}

M.use_lazy_file = true
M.lazy_file_events = { "BufReadPost", "BufNewFile" }

function M.setup()
  M.bootstrap()

  require("lazy").setup(require("config.lazy"))

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

  if M.use_lazy_file then M.lazy_file() end
end

M.inited = false
function M.init()
  if not M.inited then
    M.inited = true
    M.use_lazy_file = M.use_lazy_file and vim.fn.argc(-1) > 0
    ---@diagnostic disable-next-line: undefined-field
    M.use_lazy_file = M.use_lazy_file and require("lazy.core.handler.event").trigger_events == nil
    require("util.lazy").delay_notify()
    M.load("options")
    local LazyPlugin = require("lazy.core.plugin")
    local add = LazyPlugin.Spec.add
    ---@diagnostic disable-next-line: duplicate-set-field
    LazyPlugin.Spec.add = function(self, plugin, ...)
      if type(plugin) == "table" then
        if not M.use_lazy_file and plugin.event then
          if plugin.event == "LazyFile" then
            plugin.event = M.lazy_file_events
          elseif type(plugin.event) == "table" then
            local events = {}
            for _, event in ipairs(plugin.event) do
              if event == "LazyFile" then
                vim.list_extend(events, M.lazy_file_events)
              else
                events[#events + 1] = event
              end
            end
          end
        end
      end
      return add(self, plugin, ...)
    end

    local LazyEvent = require("lazy.core.handler.event")
    local _event = LazyEvent._event
    ---@diagnostic disable-next-line: duplicate-set-field
    LazyEvent._event = function(self, value)
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
    local LazyEvent = require("lazy.core.handler.event")
    local LazyUtil = require("lazy.core.util")
    vim.api.nvim_del_augroup_by_name("lazy_file")

    LazyUtil.track({ event = "Core.lazy_file" })

    local skips = {}
    for _, event in ipairs(events) do
      skips[event.event] = skips[event.event] or LazyEvent.get_augroups(event.event)
    end

    vim.api.nvim_exec_autocmds("User", { pattern = "LazyFile", modeline = false })
    for _, event in ipairs(events) do
      LazyEvent.trigger({
        event = event.event,
        exclude = skips[event.event],
        data = event.data,
        buf = event.buf,
      })
      if vim.bo[event.buf].filetype then
        LazyEvent.trigger({
          event = "FileType",
          buf = event.buf,
        })
      end
    end
    vim.api.nvim_exec_autocmds("CursorMoved", { modeline = false })
    events = {}
    LazyUtil.track()
  end

  load = vim.schedule_wrap(load)

  vim.api.nvim_create_autocmd(M.lazy_file_events, {
    group = vim.api.nvim_create_augroup("lazy_file", { clear = true }),
    callback = function(event)
      table.insert(events, event)
      load()
    end,
  })
end

return M
