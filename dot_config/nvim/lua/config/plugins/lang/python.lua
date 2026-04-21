-- lua/config/plugins/lang/python.lua
if vim.fn.executable("python3") == 0 then return {} end

Lib.mason.add("ruff")
Lib.mason.add("debugpy")

Lib.neotest.add("neotest-python", function() return require("neotest-python") end)

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "python", "ninja", "rst" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("ruff", {
    capabilities = Lib.lsp.capabilities(),
    cmd_env = { RUFF_TRACE = "messages" },
    init_options = {
      settings = {
        logLevel = "error",
      },
    },
  })
  Lib.lsp.enable("ruff")
end)

-- Disable ruff hover so pyright/basedpyright (if present) wins hover, matching
-- old config's setup() hook.
Lib.lsp.on_attach(function(args)
  local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
  if not client or client.name ~= "ruff" then return end
  client.server_capabilities.hoverProvider = false
end)

return {
  {
    "linux-cultist/venv-selector.nvim",
    name = "venv-selector.nvim",
    branch = "regexp",
    cmd = "VenvSelect",
    ft = "python",
    keys = { { "<Leader>cv", "<Cmd>VenvSelect<CR>", desc = "Select Virtualenv", ft = "python" } },
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
    name = "nvim-dap-python",
    ft = "python",
    dependencies = { "nvim-dap" },
    config = function()
      require("dap-python").setup(vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python")
    end,
  },
  {
    "nvim-neotest/neotest-python",
    name = "neotest-python",
    ft = "python",
    dependencies = { "neotest" },
  },
}
