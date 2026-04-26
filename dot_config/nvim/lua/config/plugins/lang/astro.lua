-- lua/config/plugins/lang/astro.lua
-- Note: old config wired an `@astrojs/ts-plugin` into vtsls for in-TS-file
-- Astro-aware completions. Skipped per M4 simplification — astro-ls alone
-- handles .astro files; TS in Astro projects loses deep Astro intelligence
-- until we revisit vtsls coordination.
return Lib.lang.setup({
  cmd = "node",
  mason = { "astro-language-server" },
  parsers = { "astro", "css" },
  servers = { astro = {} },
  formatters = { astro = { "prettier" } },
})
