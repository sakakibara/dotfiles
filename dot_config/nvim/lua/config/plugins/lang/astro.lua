-- lua/config/plugins/lang/astro.lua
if vim.fn.executable("node") == 0 then return {} end

Lib.mason.add("astro-language-server")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "astro", "css" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("astro", { capabilities = Lib.lsp.capabilities() })
  Lib.lsp.enable("astro")
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.astro = { "prettier" }
end)

-- Note: old config wired an `@astrojs/ts-plugin` into vtsls for in-TS-file
-- Astro-aware completions. Skipped per M4 simplification — astro-ls alone
-- handles .astro files; TS in Astro projects loses deep Astro intelligence
-- until we revisit vtsls coordination.

return {}
