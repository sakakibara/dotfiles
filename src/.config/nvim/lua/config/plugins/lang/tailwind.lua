-- Note: old config added `roobert/tailwindcss-colorizer-cmp.nvim` as an
-- nvim-cmp source formatter. Skipped — M2 uses blink.cmp and there is no
-- direct blink-native equivalent yet. Revisit if tailwind color swatches
-- in completion are needed.
local exclude = { markdown = true }

local default_filetypes = {
  "aspnetcorerazor", "astro", "astro-markdown", "blade", "clojure", "django-html",
  "htmldjango", "edge", "eelixir", "elixir", "ejs", "erb", "eruby", "gohtml",
  "gohtmltmpl", "haml", "handlebars", "hbs", "html", "html-eex", "heex", "jade",
  "leaf", "liquid", "markdown", "mdx", "mustache", "njk", "nunjucks", "php",
  "razor", "slim", "twig", "css", "less", "postcss", "sass", "scss", "stylus",
  "sugarss", "javascript", "javascriptreact", "reason", "rescript", "typescript",
  "typescriptreact", "vue", "svelte", "templ",
}

local function served(filetypes)
  return vim.tbl_filter(function(ft) return not exclude[ft] end, filetypes)
end

return Lib.lang.setup({
  cmd = "node",
  ft = served(default_filetypes),
  mason = { "tailwindcss-language-server" },
  servers = {
    tailwindcss = function()
      local ok, tw = pcall(require, "lspconfig.configs.tailwindcss")
      local from_lspconfig = ok and tw.default_config and tw.default_config.filetypes
      return {
        binary = "tailwindcss-language-server",
        filetypes = served(from_lspconfig or default_filetypes),
      }
    end,
  },
})
