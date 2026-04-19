-- lua/config/plugins/lang/json.lua
Lib.mason.add("json-lsp")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "json", "json5", "jsonc" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  local schemastore_ok, schemastore = pcall(require, "schemastore")
  vim.lsp.config("jsonls", {
    capabilities = Lib.lsp.capabilities(),
    settings = {
      json = {
        schemas = schemastore_ok and schemastore.json.schemas() or {},
        validate = { enable = true },
        format = { enable = true },
      },
    },
  })
  vim.lsp.enable("jsonls")
end)

return {
  { "b0o/SchemaStore.nvim", name = "SchemaStore.nvim", lazy = true },
}
