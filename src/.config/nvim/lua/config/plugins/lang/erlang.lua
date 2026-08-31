return Lib.lang.setup({
  cmd = "erl",
  ft = "erlang",
  mason = { "erlang-ls" },
  parsers = { "erlang" },
  servers = { erlangls = {} },
})
