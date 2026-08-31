return Lib.lang.setup({
  ft = { "css", "scss" },
  mason = { "css-lsp", "prettier" },
  parsers = { "css", "scss" },
  servers = {
    cssls = {
      binary = "vscode-css-language-server",
    },
  },
  formatters = {
    css = { "prettier" },
    scss = { "prettier" },
  },
})
