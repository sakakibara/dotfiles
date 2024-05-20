return {
  {
    "sakakibara/vim-ps1",
    ft = "ps1",
  },

  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { "powershell-editor-services" })
    end,
  },

  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts = vim.tbl_deep_extend("force", opts, {
        servers = {
          powershell_es = {
            bundle_path = vim.fn.stdpath("data") .. "/mason/packages/powershell-editor-services",
          },
        },
      })
    end,
  },
}
