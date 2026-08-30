return Lib.lang.setup({
  mason = { "html-lsp" },
  parsers = { "html", "css", "javascript" },
  servers = {
    html = {
      binary = "vscode-html-language-server",
    },
  },
})
