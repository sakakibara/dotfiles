return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "zig" } },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        zls = {},
      },
    },
  },

  {
    "nvim-neotest/neotest",
    optional = true,
    dependencies = {
      "lawrence-laz/neotest-zig",
    },
    opts = {
      adapters = {
        ["neotest-zig"] = {},
      },
    },
  },
}
