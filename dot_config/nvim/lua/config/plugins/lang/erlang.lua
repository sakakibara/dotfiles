-- lua/config/plugins/lang/erlang.lua
return Lib.lang.setup({
  cmd = "erl",
  mason = { "erlang-ls" },
  parsers = { "erlang" },
  servers = { erlangls = {} },
})
