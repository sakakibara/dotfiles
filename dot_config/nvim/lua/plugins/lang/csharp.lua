return {
  {
    "Hoffs/omnisharp-extended-lsp.nvim",
    lazy = true,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "c_sharp" })
      end
    end,
  },

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        cs = { "csharpier" },
      },
      formatters = {
        csharpier = {
          command = "dotnet-csharpier",
          args = { "--write-stdout" },
        },
      },
    },
  },

  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      table.insert(opts.ensure_installed, "csharpier")
    end,
  },

  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts = vim.tbl_deep_extend("force", opts, {
        servers = {
          omnisharp = {
            handlers = {
              ["textDocument/definition"] = function(...)
                return require("omnisharp_extended").handler(...)
              end,
            },
            keys = {
              {
                "gd",
                function()
                  require("omnisharp_extended").telescope_lsp_definitions()
                end,
                desc = "Goto Definition",
              },
            },
            enable_roslyn_analyzers = true,
            organize_imports_on_format = true,
            enable_import_completion = true,
            filetypes = { "cs" },
          },
        },
      })
    end,
  },
}
