return Lib.lang.setup({
  cmd = "python3",
  mason = { "ruff", "debugpy" },
  parsers = { "python", "ninja", "rst" },
  servers = {
    ruff = {
      cmd_env = { RUFF_TRACE = "messages" },
      init_options = {
        settings = {
          logLevel = "error",
        },
      },
      -- Disable ruff hover so pyright/basedpyright (if present) wins hover,
      -- matching old config's setup() hook.
      on_attach = function(_, client)
        client.server_capabilities.hoverProvider = false
      end,
    },
  },
  neotest = { ["neotest-python"] = function() return require("neotest-python") end },
  plugins = {
    {
      "linux-cultist/venv-selector.nvim",
      branch = "regexp",
      cmd = "VenvSelect",
      ft = "python",
      keys = { { "<Leader>cv", "<Cmd>VenvSelect<CR>", desc = "Select virtualenv", ft = "python" } },
      opts = {
        settings = {
          options = {
            notify_user_on_venv_activation = true,
          },
        },
      },
    },
    {
      "mfussenegger/nvim-dap-python",
      ft = "python",
      dependencies = { "nvim-dap" },
      config = function()
        require("dap-python").setup(vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python")
      end,
    },
    {
      "nvim-neotest/neotest-python",
      ft = "python",
      dependencies = { "neotest" },
    },
  },
})
