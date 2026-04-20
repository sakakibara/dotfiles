-- lua/config/plugins/lang/nushell.lua
if vim.fn.executable("nu") == 0 then return {} end

-- `nu --lsp` is built into the nushell runtime; no separate binary and no mason entry.

-- tree-sitter-nu isn't in nvim-treesitter's default registry — register it
-- manually before install() runs. On the main branch, the parsers module
-- returns the parser table directly; the old get_parser_configs() wrapper
-- and `files`/`filetype` fields are gone.
Lib.plugin.on_load("nvim-treesitter", function()
  ---@diagnostic disable-next-line: inject-field
  require("nvim-treesitter.parsers").nu = {
    install_info = {
      url = "https://github.com/nushell/tree-sitter-nu",
      revision = "main",
    },
  }
  require("nvim-treesitter").install({ "nu" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("nushell", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("nushell")
end)

return {}
