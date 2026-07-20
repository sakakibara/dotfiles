-- `nu --lsp` is built into the nushell runtime; no separate binary and no mason entry.
return Lib.lang.setup({
  cmd = "nu",
  parsers = { "nu" },
  parsers_setup = function()
    -- tree-sitter-nu isn't in nvim-treesitter's default registry — register it
    -- manually before install() runs. On the main branch, the parsers module
    -- returns the parser table directly; the old get_parser_configs() wrapper
    -- and `files`/`filetype` fields are gone.
    ---@diagnostic disable-next-line: inject-field
    require("nvim-treesitter.parsers").nu = {
      install_info = {
        url = "https://github.com/nushell/tree-sitter-nu",
        revision = "main",
      },
    }
  end,
  servers = { nushell = {} },
})
