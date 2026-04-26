-- lua/config/plugins/lang/tailwind.lua
-- Note: old config added `roobert/tailwindcss-colorizer-cmp.nvim` as an
-- nvim-cmp source formatter. Skipped — M2 uses blink.cmp and there is no
-- direct blink-native equivalent yet. Revisit if tailwind color swatches
-- in completion are needed.
return Lib.lang.setup({
  cmd = "node",
  mason = { "tailwindcss-language-server" },
  servers = {
    tailwindcss = function()
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

      return { filetypes = filetypes }
    end,
  },
})
