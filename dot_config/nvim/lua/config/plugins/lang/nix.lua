-- lua/config/plugins/lang/nix.lua
-- nixfmt isn't in the mason registry; install via
-- `nix profile install nixpkgs#nixfmt-classic`.
return Lib.lang.setup({
  cmd = "nix",
  mason = { "nil" },
  parsers = { "nix" },
  servers = { nil_ls = {} },
  formatters = { nix = { "nixfmt" } },
})
