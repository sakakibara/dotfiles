-- lua/config/plugins/lang/vue.lua
-- Note: old config coordinated vue_ls with vtsls via `@vue/typescript-plugin`
-- for hybrid mode. Skipped per M4 simplification — vue_ls alone handles .vue
-- files; TS support in Vue projects loses deep Vue intelligence until we
-- revisit vtsls coordination (tier-5 typescript).
return Lib.lang.setup({
  cmd = "node",
  mason = { "vue-language-server" },
  parsers = { "vue", "css" },
  servers = { vue_ls = {} },
})
