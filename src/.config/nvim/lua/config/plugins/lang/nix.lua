return Lib.lang.setup({
  cmd = "nix",
  mason = { "nil", "nixfmt" },
  parsers = { "nix" },
  servers = { nil_ls = {} },
  formatters = { nix = { "nixfmt" } },
})
