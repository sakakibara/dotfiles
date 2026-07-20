return Lib.lang.setup({
  cmd = "erl",
  mason = { "erlang-ls" },
  parsers = { "erlang" },
  servers = { erlangls = {} },
})
