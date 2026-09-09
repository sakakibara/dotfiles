-- vue_ls alone serves .vue files. TypeScript files in a Vue project get no
-- Vue-aware intelligence: that needs `@vue/typescript-plugin` coordinated
-- with vtsls in hybrid mode, which this config does not do.
return Lib.lang.setup({
  cmd = "node",
  ft = "vue",
  mason = { "vue-language-server" },
  parsers = { "vue", "css" },
  servers = { vue_ls = {} },
})
