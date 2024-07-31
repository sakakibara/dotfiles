return {
  "catppuccin/nvim",
  lazy = true,
  name = "catppuccin",
  opts = {
    flavour = "mocha",
    background = {
      light = "latte",
      dark = "mocha",
    },
    dim_inactive = {
      enabled = true,
      shade = "dark",
      percentage = 0.70,
    },
    integrations = {
      aerial = true,
      cmp = true,
      dap = {
        enabled = true,
        enable_ui = true,
      },
      dashboard = true,
      flash = true,
      gitsigns = true,
      headlines = true,
      illuminate = true,
      indent_blankline = { enabled = true },
      markdown = true,
      mason = true,
      mini = true,
      neotest = true,
      notify = true,
      overseer = true,
      telescope = { enabled = true },
      lsp_trouble = true,
      treesitter = true,
      ufo = true,
      which_key = true,
    },
    custom_highlights = function(colors)
      return {
        Folded = { bg = colors.mantle },
      }
    end,
  },
}
