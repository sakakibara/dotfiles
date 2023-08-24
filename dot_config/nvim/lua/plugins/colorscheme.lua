return {
  "catppuccin/nvim",
  lazy = false,
  priority = 1000,
  name = "catppuccin",
  opts = {
    flavour = jit.os:find("Linux") and "latte" or "mocha",
    background = {
        light = "latte",
        dark = "mocha",
    },
    show_end_of_buffer = true,
    integrations = {
      cmp = true,
      flash = true,
      gitsigns = true,
      illuminate = true,
      indent_blankline = { enabled = true },
      markdown = true,
      mason = true,
      navic = { enabled = true, custom_bg = "lualine" },
      neotree = true,
      notify = true,
      telescope = true,
      treesitter = true,
      which_key = true,
    },
    custom_highlights = function(colors)
      return {
        Folded = { bg = colors.crust },
      }
    end,
  },
  config = function(_, opts)
    require("catppuccin").setup(opts)
    vim.cmd.colorscheme("catppuccin")
  end
}
