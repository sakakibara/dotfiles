return Lib.lang.setup({
  ft = "html",
  mason = { "html-lsp", "prettier" },
  parsers = { "html", "css", "javascript" },
  servers = {
    html = {
      binary = "vscode-html-language-server",
    },
  },
  formatters = { html = { "prettier" } },
})
