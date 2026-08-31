-- `nu --lsp` is built into the nushell runtime; no separate binary and no mason entry.
return Lib.lang.setup({
  cmd = "nu",
  ft = "nu",
  parsers = { "nu" },
  servers = { nushell = {} },
})
