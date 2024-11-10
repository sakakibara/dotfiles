return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "ninja", "rst" } },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        ruff = {
          enabled = true,
          cmd_env = { RUFF_TRACE = "messages" },
          init_options = {
            settings = {
              logLevel = "error",
            },
          },
          keys = {
            {
              "<leader>co",
              Util.lsp.action["source.organizeImports"],
              desc = "Organize Imports",
            },
          },
        },
      },
      setup = {
        ruff = function()
          Util.lsp.on_attach(function(client, _)
            client.server_capabilities.hoverprovider = false
          end, "ruff_lsp")
        end,
      },
    },
  },

  {
    "nvim-neotest/neotest",
    optional = true,
    dependencies = {
      "nvim-neotest/neotest-python",
    },
    opts = {
      adapters = {
        ["neotest-python"] = {},
      },
    },
  },

  {
    "mfussenegger/nvim-dap",
    optional = true,
    dependencies = {
      {
        "williamboman/mason.nvim",
        opts = function(_, opts)
          opts.ensure_installed = opts.ensure_installed or {}
          table.insert(opts.ensure_installed, "debugpy")
        end,
      },
      {
        "mfussenegger/nvim-dap-python",
        keys = {
          {
            "<leader>dPt",
            function()
              require("dap-python").test_method()
            end,
            desc = "Debug method",
            ft = "python",
          },
          {
            "<leader>dPc",
            function()
              require("dap-python").test_class()
            end,
            desc = "Debug class",
            ft = "python",
          },
        },
        config = function()
          if Util.is_win then
            require("dap-python").setup(Util.lsp.get_pkg_path("debugpy", "/venv/Scripts/pythonw.exe"))
          else
            require("dap-python").setup(Util.lsp.get_pkg_path("debugpy", "/venv/bin/python"))
          end
        end,
      },
    },
  },

  {
    "linux-cultist/venv-selector.nvim",
    branch = "regexp",
    cmd = "VenvSelect",
    opts = {
      settings = {
        options = {
          notify_user_on_venv_activation = true,
        },
      },
    },
    ft = "python",
    keys = { { "<Leader>cv", "<Cmd>:VenvSelect<cr>", desc = "Select virtualenv", ft = "python" } },
  },

  {
    "jay-babu/mason-nvim-dap.nvim",
    optional = true,
    opts = {
      handlers = {
        python = function() end,
      },
    },
  },
}
