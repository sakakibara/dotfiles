-- lua/config/plugins/lang/go.lua
if vim.fn.executable("go") == 0 then return {} end

Lib.mason.add("gopls", "goimports", "gofumpt")
Lib.mason.add("delve")

Lib.neotest.add("neotest-go", function() return require("neotest-go") end)

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "go", "gomod", "gowork", "gosum" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("gopls", {
    capabilities = Lib.lsp.capabilities(),
    settings = {
      gopls = {
        gofumpt = true,
        codelenses = {
          gc_details = false,
          generate = true,
          regenerate_cgo = true,
          run_govulncheck = true,
          test = true,
          tidy = true,
          upgrade_dependency = true,
          vendor = true,
        },
        hints = {
          assignVariableTypes = true,
          compositeLiteralFields = true,
          compositeLiteralTypes = true,
          constantValues = true,
          functionTypeParameters = true,
          parameterNames = true,
          rangeVariableTypes = true,
        },
        analyses = {
          fieldalignment = true,
          nilness = true,
          unusedparams = true,
          unusedwrite = true,
          useany = true,
        },
        usePlaceholders = true,
        completeUnimported = true,
        staticcheck = true,
        directoryFilters = { "-.git", "-.vscode", "-.idea", "-.vscode-test", "-node_modules" },
        semanticTokens = true,
      },
    },
  })
  vim.lsp.enable("gopls")
end)

-- Populate semanticTokensProvider from negotiated capabilities if the server
-- didn't advertise one — matches the old setup() hook behavior.
Lib.lsp.on_attach(function(args)
  local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
  if not client or client.name ~= "gopls" then return end
  if not client.server_capabilities.semanticTokensProvider then
    local semantic = client.config.capabilities
      and client.config.capabilities.textDocument
      and client.config.capabilities.textDocument.semanticTokens
    if semantic then
      client.server_capabilities.semanticTokensProvider = {
        full = true,
        legend = {
          tokenTypes = semantic.tokenTypes,
          tokenModifiers = semantic.tokenModifiers,
        },
        range = true,
      }
    end
  end
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.go = { "goimports", "gofumpt" }
end)

return {
  {
    "leoluz/nvim-dap-go",
    name = "nvim-dap-go",
    ft = "go",
    dependencies = { "nvim-dap" },
    config = function() require("dap-go").setup() end,
  },
  {
    "nvim-neotest/neotest-go",
    name = "neotest-go",
    ft = "go",
    dependencies = { "neotest" },
  },
}
