return Lib.lang.setup({
  mason = { "html-lsp", "prettier" },
  parsers = { "html", "css", "javascript" },
  servers = {
    html = {
      binary = "vscode-html-language-server",
    },
  },
  formatters = { html = { "prettier" } },
})
