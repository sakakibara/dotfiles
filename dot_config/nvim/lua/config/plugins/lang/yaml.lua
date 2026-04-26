-- lua/config/plugins/lang/yaml.lua
return Lib.lang.setup({
  mason = { "yaml-language-server" },
  parsers = { "yaml" },
  servers = {
    yamlls = function()
      local schemastore_ok, schemastore = pcall(require, "schemastore")
      return {
        capabilities = {
          textDocument = {
            foldingRange = {
              dynamicRegistration = false,
              lineFoldingOnly = true,
            },
          },
        },
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
        _enable = { cmd = "yaml-language-server" },  -- function cmd in lspconfig
      }
    end,
  },
  plugins = {
    { "b0o/SchemaStore.nvim", name = "SchemaStore.nvim", lazy = true },
  },
})
