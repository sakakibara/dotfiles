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

      vim.api.nvim_create_user_command("Root", function()
        require("util.root").info()
      end, { desc = "Show root info for the current buffer" })
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

M.use_lazy_file = true
M.lazy_file_events = { "BufReadPost", "BufNewFile", "BufWritePre" }

function M.lazy_file()
  M.use_lazy_file = M.use_lazy_file and vim.fn.argc(-1) > 0

  local LazyEvent = require("lazy.core.handler.event")

  if M.use_lazy_file then
    LazyEvent.mappings.LazyFile = { id = "LazyFile", event = "User", pattern = "LazyFile" }
    LazyEvent.mappings["User LazyFile"] = LazyEvent.mappings.LazyFile
  else
    LazyEvent.mappings.LazyFile = { id = "LazyFile", event = M.lazy_file_events }
    LazyEvent.mappings["User LazyFile"] = LazyEvent.mappings.LazyFile
    return
  end

  local events = {}

  local function load()
    if #events == 0 then
      return
    end

    vim.api.nvim_del_augroup_by_name("lazy_file")

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
