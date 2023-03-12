return {
  "catppuccin/nvim",
  lazy = false,
  priority = 1000,
  name = "catppuccin",
  opts = { flavour = "mocha" },
  config = function()
    vim.cmd.colorscheme("catppuccin")
  end
}
