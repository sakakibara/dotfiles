return Lib.lang.setup({
  cmd = "erl",
  ft = "erlang",
  mason = { "elp" },
  parsers = { "erlang" },
  servers = { elp = {} },
})
