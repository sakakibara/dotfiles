-- lua/config/plugins/lang/go.lua
if vim.fn.executable("go") == 0 then return {} end

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

return Lib.lang.setup({
  mason = { "gopls", "goimports", "gofumpt", "delve" },
  parsers = { "go", "gomod", "gowork", "gosum" },
  servers = {
    gopls = {
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
    },
  },
  formatters = { go = { "goimports", "gofumpt" } },
  neotest = { ["neotest-go"] = function() return require("neotest-go") end },
  plugins = {
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
  },
})
