return Lib.lang.setup({
  ft = { "css", "scss" },
  mason = { "css-lsp" },
  parsers = { "css", "scss" },
  servers = {
    cssls = {
      binary = "vscode-css-language-server",
    },
  },
})
