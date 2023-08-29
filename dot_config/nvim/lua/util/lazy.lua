local M = {}

function M.has(plugin)
  return require("lazy.core.config").spec.plugins[plugin] ~= nil
end

function M.opts(name)
  local plugin = require("lazy.core.config").plugins[name]
  if not plugin then
    return {}
  end
  local Plugin = require("lazy.core.plugin")
  return Plugin.values(plugin, "opts", false)
end

function M.on_load(name, fn)
  local Config = require("lazy.core.config")
  if Config.plugins[name] and Config.plugins[name]._.loaded then
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

function M.delay_notify()
  local notifs = {}
  local function temp(...)
    table.insert(notifs, vim.F.pack_len(...))
  end
  local orig = vim.notify
  vim.notify = temp
  local timer = vim.loop.new_timer()
  local check = vim.loop.new_check()
  local replay = function()
    timer:stop()
    if check then check:stop() end
    if vim.notify == temp then
      vim.notify = orig
    end
    vim.schedule(function()
      for _, notif in ipairs(notifs) do
        vim.notify(vim.F.unpack_len(notif))
      end
    end)
  end
  if check then
    check:start(function()
      if vim.notify ~= temp then
        replay()
      end
    end)
  end
  timer:start(500, 0, replay)
end

return M
