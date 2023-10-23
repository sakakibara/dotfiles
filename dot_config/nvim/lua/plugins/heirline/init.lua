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

return {
  {
    "rebelot/heirline.nvim",
    event = "VeryLazy",
    init = function()
      if vim.fn.argc(-1) > 0 then
        require("util.plugin").on_very_lazy(function()
          for _, win in ipairs(vim.api.nvim_list_wins()) do
            vim.wo[win].winbar = "%{%v:lua.require'heirline'.eval_winbar()%}"
            vim.wo[win].statuscolumn = "%{%v:lua.require'heirline'.eval_statuscolumn()%}"
          end
        end)
      end
    end,
    opts = function()
      local conditions = require("heirline.conditions")
      return {
        statusline = require("plugins.heirline.statusline"),
        winbar = require("plugins.heirline.winbar"),
        statuscolumn = require("plugins.heirline.statuscolumn"),
        opts = {
          disable_winbar_cb = function(args)
            return conditions.buffer_matches({
              buftype = { "nofile", "prompt", "help", "quickfix" },
              filetype = { "Trouble", "neo-tree" },
            }, args.buf)
          end,
        },
      }
    end,
    config = function(_, opts)
      local heirline = require("heirline")
      heirline.load_colors(M.setup_colors())
      heirline.setup(opts)

      vim.api.nvim_create_autocmd("ColorScheme", {
        group = vim.api.nvim_create_augroup("Heirline", { clear = true }),
        callback = function()
          require("heirline.utils").on_colorscheme(M.setup_colors)
        end,
      })
    end,
  },
}
