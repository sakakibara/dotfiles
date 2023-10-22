return {
  {
    "rebelot/heirline.nvim",
    event = "BufEnter",
    init = function()
      vim.g.heirline_laststatus = vim.o.laststatus
      if vim.fn.argc(-1) > 0 then
        vim.o.statusline = " "
      else
        vim.o.laststatus = 0
      end
    end,
    config = function()
      vim.o.laststatus = vim.g.heirline_laststatus

      local conditions = require("heirline.conditions")
      local utils = require("heirline.utils")

      local function setup_colors()
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

      require("heirline").setup({
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
          colors = setup_colors,
        },
      })

      vim.api.nvim_create_autocmd("ColorScheme", {
        group = vim.api.nvim_create_augroup("Heirline", { clear = true }),
        callback = function()
          require("heirline.utils").on_colorscheme(setup_colors)
        end,
      })
    end,
  },
}
