-- vue_ls serves the template and style blocks of .vue files; vtsls (with
-- `@vue/typescript-plugin`, configured in typescript.lua) serves their scripts.
return Lib.lang.setup({
  cmd = "node",
  ft = "vue",
  mason = { "vue-language-server", "vtsls" },
  parsers = { "vue", "css" },
  servers = { vue_ls = {} },
})
