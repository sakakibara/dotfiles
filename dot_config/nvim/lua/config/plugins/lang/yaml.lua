-- lua/config/plugins/lang/yaml.lua
Lib.mason.add("yaml-language-server")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "yaml" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  local schemastore_ok, schemastore = pcall(require, "schemastore")
  local caps = Lib.lsp.capabilities()
  caps = vim.tbl_deep_extend("force", caps, {
    textDocument = {
      foldingRange = {
        dynamicRegistration = false,
        lineFoldingOnly = true,
      },
    },
  })
  vim.lsp.config("yamlls", {
    capabilities = caps,
    settings = {
      redhat = { telemetry = { enabled = false } },
      yaml = {
        keyOrdering = false,
        format = { enable = true },
        validate = true,
        schemaStore = {
          enable = false,
          url = "",
        },
        schemas = schemastore_ok and schemastore.yaml.schemas() or {},
      },
    },
  })
  Lib.lsp.enable("yamlls", { cmd = "yaml-language-server" })  -- function cmd in lspconfig
end)

return {
  { "b0o/SchemaStore.nvim", name = "SchemaStore.nvim", lazy = true },
}
