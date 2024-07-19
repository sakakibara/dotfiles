local M = {}

function M.setup_colors()
  local utils = require("heirline.utils")
  return {
    bright_bg = utils.get_highlight("Folded").bg,
    bright_fg = utils.get_highlight("Folded").fg,
    red = utils.get_highlight("DiagnosticError").fg,
    dark_red = utils.get_highlight("DiffDelete").bg,
    green = utils.get_highlight("String").fg,
    blue = utils.get_highlight("Function").fg,
    gray = utils.get_highlight("NonText").fg,
    orange = utils.get_highlight("Constant").fg,
    purple = utils.get_highlight("Statement").fg,
    cyan = utils.get_highlight("Special").fg,
    diag_warn = utils.get_highlight("DiagnosticWarn").fg,
    diag_error = utils.get_highlight("DiagnosticError").fg,
    diag_hint = utils.get_highlight("DiagnosticHint").fg,
    diag_info = utils.get_highlight("DiagnosticInfo").fg,
    git_del = utils.get_highlight("diffDeleted").fg,
    git_add = utils.get_highlight("diffAdded").fg,
    git_change = utils.get_highlight("diffChanged").fg,
  }
end

function M.can_set_option_value(name, win)
  local win_id = win or 0
  local ret = true
  if name == "winbar" and vim.api.nvim_win_get_height(win_id) <= 1 then
    ret = false
  end
  return ret
end

function M.set_option_value_cb(name, value, callback)
  local params = { name = name, value = value, callback = callback }
  return function(args)
    name, value, callback = params.name, params.value, params.callback
    if args.event == "VimEnter" or args.event == "UIEnter" then
      for _, win in ipairs(vim.api.nvim_list_wins()) do
        local winbuf = vim.api.nvim_win_get_buf(win)
        local new_args = vim.deepcopy(args)
        new_args.buf = winbuf
        if callback and callback(new_args) == true then
          vim.api.nvim_set_option_value(name, nil, { win = win })
        else
          if M.can_set_option_value(name, win) then
            vim.api.nvim_set_option_value(name, value, { win = win })
          end
        end
      end
    end
    if callback and callback(args) == true then
      vim.api.nvim_set_option_value(name, nil, { scope = "local" })
    else
      if M.can_set_option_value(name) then
        vim.api.nvim_set_option_value(name, value, { scope = "local" })
      end
    end
  end
end

function M.setup_option(name, value, callback)
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    local winbuf = vim.api.nvim_win_get_buf(win)
    local args = { buf = winbuf }
    if callback and callback(args) == true then
      vim.api.nvim_set_option_value(name, nil, { win = win })
    else
      if M.can_set_option_value(name, win) then
        vim.api.nvim_set_option_value(name, value, { win = win })
      end
    end
  end
  local augroup = vim.api.nvim_create_augroup("userconf_heirline_" .. name .. "_update", { clear = true })
  vim.api.nvim_create_autocmd({ "VimEnter", "UIEnter", "BufWinEnter", "FileType", "TermOpen" }, {
    group = augroup,
    callback = M.set_option_value_cb(name, value, callback),
    desc = "Heirline update window local option",
  })
end

---@param config {statusline: StatusLine, winbar: StatusLine, tabline: StatusLine, statuscolumn: StatusLine, opts: table}
function M.setup(config)
  local heirline = require("heirline")
  local StatusLine = require("heirline.statusline")

  vim.g.qf_disable_statusline = true
  vim.api.nvim_create_augroup("Heirline_update_autocmds", { clear = true })
  heirline.reset_highlights()

  local opts = config.opts or {}

  if opts.colors then
    heirline.load_colors(opts.colors)
  end

  if config.statusline then
    heirline.statusline = StatusLine:new(config.statusline)
    vim.o.statusline = "%{%v:lua.require'heirline'.eval_statusline()%}"
  end

  if config.winbar then
    heirline.winbar = StatusLine:new(config.winbar)
    M.setup_option("winbar", "%{%v:lua.require'heirline'.eval_winbar()%}", opts.disable_winbar_cb)
  end

  if config.tabline then
    heirline.tabline = StatusLine:new(config.tabline)
    vim.o.tabline = "%{%v:lua.require'heirline'.eval_tabline()%}"
  end

  if config.statuscolumn then
    heirline.statuscolumn = StatusLine:new(config.statuscolumn)
    M.setup_option("statuscolumn", "%{%v:lua.require'heirline'.eval_statuscolumn()%}", opts.disable_statuscolumn_cb)
  end
end

return {
  {
    "rebelot/heirline.nvim",
    event = "VeryLazy",
    init = function()
      vim.g.heirline_laststatus = vim.o.laststatus
      if vim.fn.argc(-1) > 0 then
        vim.o.statusline = " "
      else
        vim.o.laststatus = 0
      end
    end,
    opts = function()
      vim.o.laststatus = vim.g.heirline_laststatus
      local special_buffers = {
        buftype = { "nofile", "prompt", "help", "quickfix" },
        filetype = { "Trouble", "NvimTree", "dashboard", "fzf" },
      }
      return {
        statusline = require("plugins.heirline.statusline"),
        -- winbar = require("plugins.heirline.winbar"),
        statuscolumn = require("plugins.heirline.statuscolumn"),
        opts = {
          disable_winbar_cb = function(args)
            return require("heirline.conditions").buffer_matches({
              filetype = { "fzf", "lazy", "mason", "notify", "oil_preview" },
            }, args.buf)
          end,
          disable_statuscolumn_cb = function(args)
            return require("heirline.conditions").buffer_matches(special_buffers, args.buf)
          end,
          colors = M.setup_colors(),
        },
      }
    end,
    config = function(_, opts)
      M.setup(opts)

      vim.api.nvim_create_autocmd("ColorScheme", {
        group = vim.api.nvim_create_augroup("Heirline", { clear = true }),
        callback = function()
          require("heirline.utils").on_colorscheme(M.setup_colors)
        end,
      })
    end,
  },
}
