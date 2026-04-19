-- lua/config/plugins/lang/tailwind.lua
if vim.fn.executable("node") == 0 then return {} end

Lib.mason.add("tailwindcss-language-server")

Lib.plugin.on_load("nvim-lspconfig", function()
  local ok, tw = pcall(require, "lspconfig.configs.tailwindcss")
  local default_filetypes = (ok and tw.default_config and tw.default_config.filetypes) or {
    "aspnetcorerazor", "astro", "astro-markdown", "blade", "clojure", "django-html",
    "htmldjango", "edge", "eelixir", "elixir", "ejs", "erb", "eruby", "gohtml",
    "gohtmltmpl", "haml", "handlebars", "hbs", "html", "html-eex", "heex", "jade",
    "leaf", "liquid", "markdown", "mdx", "mustache", "njk", "nunjucks", "php",
    "razor", "slim", "twig", "css", "less", "postcss", "sass", "scss", "stylus",
    "sugarss", "javascript", "javascriptreact", "reason", "rescript", "typescript",
    "typescriptreact", "vue", "svelte", "templ",
  }

  local exclude = { markdown = true }
  local filetypes = {}
  for _, ft in ipairs(default_filetypes) do
    if not exclude[ft] then table.insert(filetypes, ft) end
  end

  vim.lsp.config("tailwindcss", {
    capabilities = Lib.lsp.capabilities(),
    filetypes = filetypes,
  })
  vim.lsp.enable("tailwindcss")
end)

-- Note: old config added `roobert/tailwindcss-colorizer-cmp.nvim` as an
-- nvim-cmp source formatter. Skipped — M2 uses blink.cmp and there is no
-- direct blink-native equivalent yet. Revisit if tailwind color swatches
-- in completion are needed.

return {}
