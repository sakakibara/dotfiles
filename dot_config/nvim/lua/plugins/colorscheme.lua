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
      blink_cmp = true,
      cmp = true,
      dap = {
        enabled = true,
        enable_ui = true,
      },
      dashboard = true,
      flash = true,
      gitsigns = true,
      grug_far = true,
      illuminate = true,
      indent_blankline = { enabled = true },
      markdown = true,
      mason = true,
      mini = true,
      native_lsp = {
        enabled = true,
        underlines = {
          errors = { "undercurl" },
          hints = { "undercurl" },
          warnings = { "undercurl" },
          information = { "undercurl" },
        },
      },
      neotest = true,
      noice = true,
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
