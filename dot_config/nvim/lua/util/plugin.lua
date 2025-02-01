---@class util.plugin
local M = {}

function M.setup()
  M.lazy_file()
end

M.lazy_file_events = { "BufReadPost", "BufNewFile", "BufWritePre" }

function M.lazy_file()
  local LazyEvent = require("lazy.core.handler.event")

  LazyEvent.mappings.LazyFile = { id = "LazyFile", event = M.lazy_file_events }
  LazyEvent.mappings["User LazyFile"] = LazyEvent.mappings.LazyFile
end

function M.opts(name)
  local plugin = require("lazy.core.config").spec.plugins[name]
  if not plugin then
    return {}
  end
  local LazyPlugin = require("lazy.core.plugin")
  return LazyPlugin.values(plugin, "opts", false)
end

function M.is_loaded(name)
  local LazyConfig = require("lazy.core.config")
  return LazyConfig.plugins[name] and LazyConfig.plugins[name]._.loaded
end

function M.on_load(name, fn)
  if M.is_loaded(name) then
    fn(name)
  else
    vim.api.nvim_create_autocmd("User", {
      pattern = "LazyLoad",
      callback = function(event)
        if event.data == name then
          fn(name)
          return true
        end
      end,
    })
  end
end

function M.get_plugin(name)
  return require("lazy.core.config").spec.plugins[name]
end

function M.get_plugin_path(name, path)
  local plugin = M.get_plugin(name)
  path = path and "/" .. path or ""
  return plugin and (plugin.dir .. path)
end

function M.has(plugin)
  return M.get_plugin(plugin) ~= nil
end

function M.on_very_lazy(fn)
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = function()
      fn()
    end,
  })
end

return M
