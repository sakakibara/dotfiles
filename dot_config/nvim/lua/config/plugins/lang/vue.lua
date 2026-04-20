-- lua/config/plugins/lang/vue.lua
if vim.fn.executable("node") == 0 then return {} end

Lib.mason.add("vue-language-server")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "vue", "css" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("vue_ls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("vue_ls")
end)

-- Note: old config coordinated vue_ls with vtsls via `@vue/typescript-plugin`
-- for hybrid mode. Skipped per M4 simplification — vue_ls alone handles .vue
-- files; TS support in Vue projects loses deep Vue intelligence until we
-- revisit vtsls coordination (tier-5 typescript).

return {}
