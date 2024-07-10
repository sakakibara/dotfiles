return {
  {
    "nvim-treesitter",
    opts = { ensure_installed = { "thrift" } },
  },

  {
    "nvim-lspconfig",
    optional = true,
    opts = {
      servers = {
        thriftls = {},
      },
    },
  },
}
