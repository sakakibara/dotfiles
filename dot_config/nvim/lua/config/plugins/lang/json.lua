-- lua/config/plugins/lang/json.lua
return Lib.lang.setup({
  mason = { "json-lsp" },
  -- nvim-treesitter main dropped the dedicated jsonc parser; the json
  -- parser handles jsonc buffers well enough.
  parsers = { "json", "json5" },
  servers = {
    jsonls = function()
      local schemastore_ok, schemastore = pcall(require, "schemastore")
      return {
        settings = {
          json = {
            schemas = schemastore_ok and schemastore.json.schemas() or {},
            validate = { enable = true },
            format = { enable = true },
          },
        },
        -- jsonls ships a function `cmd`; pass an explicit binary hint so our
        -- guarded enable can verify availability without trying to call the fn.
        binary = "vscode-json-language-server",
      }
    end,
  },
  plugins = {
    { "b0o/SchemaStore.nvim", lazy = true },
  },
})
