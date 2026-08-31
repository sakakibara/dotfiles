return Lib.lang.setup({
  cmd = "elm",
  ft = "elm",
  mason = { "elm-language-server", "elm-format" },
  parsers = { "elm" },
  servers = { elmls = {} },
  formatters = { elm = { "elm_format" } },
})
