-- lua/config/plugins/lang/json.lua
Lib.mason.add("json-lsp")

Lib.plugin.on_load("nvim-treesitter", function()
  -- nvim-treesitter main dropped the dedicated jsonc parser; the json
  -- parser handles jsonc buffers well enough.
  require("nvim-treesitter").install({ "json", "json5" })
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
  -- jsonls ships a function `cmd`; pass an explicit binary hint so our
  -- guarded enable can verify availability without trying to call the fn.
  Lib.lsp.enable("jsonls", { cmd = "vscode-json-language-server" })
end)

return {
  { "b0o/SchemaStore.nvim", name = "SchemaStore.nvim", lazy = true },
}
