-- lua/config/plugins/lang/toml.lua
return Lib.lang.setup({
  mason = { "taplo" },
  parsers = { "toml" },
  servers = { taplo = {} },
})
