return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "elm" } },
  },

  {
    "mason-org/mason.nvim",
    opts = { ensure_installed = { "elm-format" } },
  },

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        elm = { "elm_format" },
      },
    },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        elmls = {},
      },
    },
  },
}
