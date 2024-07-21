_G.Util = require("util")

local M = {}

Util.config = M

---@class ConfigOptions
local options = {
  icons = {
    dap = {
      Breakpoint = " ",
      BreakpointCondition = " ",
      BreakpointRejected = { " ", "DiagnosticError" },
      LogPoint = ".>",
      Stopped = { "󰁕 ", "DiagnosticWarn", "DapStoppedLine" },
    },
    diagnostics = {
      Error = " ",
      Hint = " ",
      Info = " ",
      Warn = " ",
    },
    git = {
      added = " ",
      modified = " ",
      removed = " ",
    },
    kinds = {
      Array = " ",
      Boolean = " ",
      Class = " ",
      Color = " ",
      Constant = " ",
      Constructor = " ",
      Copilot = " ",
      Enum = " ",
      EnumMember = " ",
      Event = " ",
      Field = " ",
      File = " ",
      Folder = " ",
      Function = " ",
      Interface = " ",
      Key = " ",
      Keyword = " ",
      Method = " ",
      Module = " ",
      Namespace = " ",
      Null = " ",
      Number = " ",
      Object = " ",
      Operator = " ",
      Package = " ",
      Property = " ",
      Reference = " ",
      Snippet = " ",
      String = " ",
      Struct = " ",
      Text = " ",
      TypeParameter = " ",
      Unit = " ",
      Value = " ",
      Variable = " ",
    },
    status = {
      Lsp = " ",
      Vim = " ",
      Mode = " ",
      Lock = " ",
      Debug = " ",
      Directory = " ",
      DirectoryAlt = "󰉖 ",
      Ellipsis = "…",
      Separator = {
        Breadcrumb = "",
      },
      Failure = " ",
      Canceled = "󰜺 ",
      Success = "󰄴 ",
      Running = "󰑮 ",
      FoldClose = "",
      FoldOpen = "",
      FoldSeparator = " ",
    },
    powerline = {
      vertical_bar_thin = "│",
      vertical_bar = "┃",
      block = "█",
      left = "",
      left_filled = "",
      right = "",
      right_filled = "",
      slant_left = "",
      slant_left_thin = "",
      slant_right = "",
      slant_right_thin = "",
      slant_left_inverse = "",
      slant_left_inverse_thin = "",
      slant_right_inverse = "",
      slant_right_inverse_thin = "",
      left_rounded = "",
      left_rounded_thin = "",
      right_rounded = "",
      right_rounded_thin = "",
      trapezoid_left = "",
      trapezoid_right = "",
      line_number = "",
      column_number = "",
    },
  },
  kind_filter = {
    default = {
      "Class",
      "Constructor",
      "Enum",
      "Field",
      "Function",
      "Interface",
      "Method",
      "Module",
      "Namespace",
      "Package",
      "Property",
      "Struct",
      "Trait",
    },
    markdown = false,
    help = false,
    lua = {
      "Class",
      "Constructor",
      "Enum",
      "Field",
      "Function",
      "Interface",
      "Method",
      "Module",
      "Namespace",
      "Property",
      "Struct",
      "Trait",
    },
  },
}

M.json = {
  path = vim.fn.stdpath("config") .. "/plugins.json",
  data = {
    version = nil,
    extras = {},
  },
}

function M.json.load()
  local f = io.open(M.json.path, "r")
  if f then
    local data = f:read("*a")
    f:close()
    local ok, json = pcall(vim.json.decode, data, { luanil = { object = true, array = true } })
    if ok then
      M.json.data = vim.tbl_deep_extend("force", M.json.data, json or {})
    end
  end
end

local clipboard

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

      if clipboard ~= nil then
        vim.opt.clipboard = clipboard
      end

      Util.format.setup()
      Util.root.setup()

      vim.api.nvim_create_user_command("Extras", function()
        Util.extras.show()
      end, { desc = "Manage extras" })

      vim.api.nvim_create_user_command("CheckHealth", function()
        vim.cmd([[Lazy! load all]])
        vim.cmd([[checkhealth]])
      end, { desc = "Load all plugins and run :checkhealth" })
    end,
  })

  Util.track("colorscheme")
  Util.try(function()
    vim.cmd.colorscheme(opts.install.colorscheme[1])
  end, {
    msg = "Failed to load the colorscheme",
    on_error = function(msg)
      Util.error(msg)
      vim.cmd.colorscheme("colorscheme")
    end,
  })
  Util.track()
end

function M.get_kind_filter(buf)
  buf = (buf == nil or buf == 0) and vim.api.nvim_get_current_buf() or buf
  local ft = vim.bo[buf].filetype
  if M.kind_filter == false then
    return
  end
  if M.kind_filter[ft] == false then
    return
  end
  if type(M.kind_filter[ft]) == "table" then
    return M.kind_filter[ft]
  end
  ---@diagnostic disable-next-line: return-type-mismatch
  return type(M.kind_filter) == "table" and type(M.kind_filter.default) == "table" and M.kind_filter.default or nil
end

function M.load(name)
  local function _load(mod)
    if require("lazy.core.cache").find(mod)[1] then
      Util.try(function()
        require(mod)
      end, { msg = "Failed to load" .. mod })
    end
  end
  _load("config." .. name)
  if vim.bo.filetype == "lazy" then
    vim.cmd([[do VimResized]])
  end
end

function M.delay_notify()
  local notifs = {}
  local function temp(...)
    table.insert(notifs, vim.F.pack_len(...))
  end
  local orig = vim.notify
  vim.notify = temp
  local timer = vim.uv.new_timer()
  local check = assert(vim.uv.new_check())
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

M.inited = false
function M.init()
  if M.inited then
    return
  end
  M.inited = true
  M.delay_notify()
  M.load("options")

  clipboard = vim.opt.clipboard
  vim.opt.clipboard = ""

  Util.plugin.setup()
  M.json.load()
end

setmetatable(M, {
  __index = function(_, key)
    ---@cast options ConfigOptions
    return options[key]
  end,
})

return M
