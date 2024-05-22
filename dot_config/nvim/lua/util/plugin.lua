local LazyUtil = require("lazy.core.util")

local M = {}

function M.setup(opts)
  require("lazy").setup(opts)

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
      require("util.root").setup()

      vim.api.nvim_create_user_command("LazyHealth", function()
        vim.cmd([[Lazy! load all]])
        vim.cmd([[checkhealth]])
      end, { desc = "Load all plugins and run :checkhealth" })
    end,
  })

  LazyUtil.track("colorscheme")
  LazyUtil.try(function()
    vim.cmd.colorscheme(opts.install.colorscheme[1])
  end, {
    msg = "Failed to load the colorscheme",
    on_error = function(msg)
      LazyUtil.error(msg)
      vim.cmd.colorscheme("colorscheme")
    end,
  })
  LazyUtil.track()
end

M.inited = false
function M.init()
  if M.inited then
    return
  end
  M.inited = true
  M.delay_notify()
  M.load("options")
  M.lazy_file()
end

function M.load(name)
  local function _load(mod)
    if require("lazy.core.cache").find(mod)[1] then
      LazyUtil.try(function()
        require(mod)
      end, { msg = "Failed to load" .. mod })
    end
  end
  _load("config." .. name)
  if vim.bo.filetype == "lazy" then
    vim.cmd([[do VimResized]])
  end
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

  local Event = require("lazy.core.handler.event")

  Event.mappings.LazyFile = { id = "LazyFile", event = M.lazy_file_events }
  Event.mappings["User LazyFile"] = Event.mappings.LazyFile
end

function M.has(name)
  return require("lazy.core.config").spec.plugins[name] ~= nil
end

function M.opts(name)
  local plugin = require("lazy.core.config").spec.plugins[name]
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

function M.on_very_lazy(fn)
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = function()
      fn()
    end,
  })
end

function M.delay_notify()
  local notifs = {}
  local function temp(...)
    table.insert(notifs, vim.F.pack_len(...))
  end
  local orig = vim.notify
  vim.notify = temp
  local timer = vim.loop.new_timer()
  local check = assert(vim.loop.new_check())
  local replay = function()
    timer:stop()
    check:stop()
    if vim.notify == temp then
      vim.notify = orig
    end
    vim.schedule(function()
      for _, notif in ipairs(notifs) do
        vim.notify(vim.F.unpack_len(notif))
      end
    end)
  end
  check:start(function()
    if vim.notify ~= temp then
      replay()
    end
  end)
  timer:start(500, 0, replay)
end

return M
