-- lua/config/plugins/lang/nushell.lua
if vim.fn.executable("nu") == 0 then return {} end

-- `nu --lsp` is built into the nushell runtime; no separate binary and no mason entry.

-- tree-sitter-nu isn't in nvim-treesitter's default registry — register it
-- manually before ensure_installed runs.
Lib.plugin.on_load("nvim-treesitter", function()
  ---@diagnostic disable-next-line: inject-field
  require("nvim-treesitter.parsers").get_parser_configs().nu = {
    install_info = {
      url = "https://github.com/nushell/tree-sitter-nu",
      files = { "src/parser.c" },
      branch = "main",
    },
    filetype = "nu",
  }
  require("nvim-treesitter.install").ensure_installed({ "nu" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("nushell", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("nushell")
end)

return {}
