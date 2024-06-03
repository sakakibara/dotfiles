---@class util.plugin
local M = {}

function M.setup()
  M.lazy_file()
end

M.lazy_file_events = { "BufReadPost", "BufNewFile", "BufWritePre" }

function M.lazy_file()
  vim.api.nvim_create_autocmd("BufReadPost", {
    once = true,
    callback = function(event)
      if vim.v.vim_did_enter == 1 then
        return
      end

      local ft = vim.filetype.match({ buf = event.buf })
      if ft then
        local lang = vim.treesitter.language.get_lang(ft)
        if not (lang and pcall(vim.treesitter.start, event.buf, lang)) then
          vim.bo[event.buf].syntax = ft
        end

        vim.cmd([[redraw]])
      end
    end,
  })

  local LazyEvent = require("lazy.core.handler.event")

  LazyEvent.mappings.LazyFile = { id = "LazyFile", event = M.lazy_file_events }
  LazyEvent.mappings["User LazyFile"] = LazyEvent.mappings.LazyFile
end

function M.has(name)
  return require("lazy.core.config").spec.plugins[name] ~= nil
end

function M.opts(name)
  local plugin = require("lazy.core.config").spec.plugins[name]
  if not plugin then
    return {}
  end
  local LazyPlugin = require("lazy.core.plugin")
  return LazyPlugin.values(plugin, "opts", false)
end

function M.on_load(name, fn)
  local LazyConfig = require("lazy.core.config")
  if LazyConfig.plugins[name] and LazyConfig.plugins[name]._.loaded then
    vim.schedule(function()
      fn(name)
    end)
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

function M.on_very_lazy(fn)
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = function()
      fn()
    end,
  })
end

function M.extend(t, key, values)
  local keys = vim.split(key, ".", { plain = true })
  for i = 1, #keys do
    local k = keys[i]
    t[k] = t[k] or {}
    if type(t) ~= "table" then
      return
    end
    t = t[k]
  end
  return vim.list_extend(t, values)
end

function M.get_pkg_path(pkg, path, opts)
  pcall(require, "mason")
  local root = vim.env.MASON or (vim.fn.stdpath("data") .. "/mason")
  opts = opts or {}
  opts.warn = opts.warn == nil and true or opts.warn
  path = path or ""
  local ret = root .. "/packages/" .. pkg .. "/" .. path
  if opts.warn and not vim.loop.fs_stat(ret) then
    Util.warn(("Mason package path not found for **%s**:\n- `%s`"):format(pkg, path))
  end
  return ret
end

return M
