-- lua/config/plugins/lang/gleam.lua
return Lib.lang.setup({
  cmd = "gleam",
  parsers = { "gleam" },
  servers = { gleam = {} },
})
