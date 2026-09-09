-- astro-ls alone serves .astro files. TypeScript files in an Astro project
-- get no Astro-aware completions: that needs `@astrojs/ts-plugin` wired
-- into vtsls, which this config does not do.
return Lib.lang.setup({
  cmd = "node",
  ft = "astro",
  mason = { "astro-language-server", "prettier" },
  parsers = { "astro", "css" },
  servers = { astro = { binary = "astro-ls" } },
  formatters = { astro = { "prettier" } },
})
