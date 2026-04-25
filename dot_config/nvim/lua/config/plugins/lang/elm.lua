-- lua/config/plugins/lang/elm.lua
-- elm-language-server and elm-format aren't in the mason registry; install via
-- `npm i -g @elm-tooling/elm-language-server` and `npm i -g elm-format`.
return Lib.lang.setup({
  cmd = "elm",
  parsers = { "elm" },
  servers = { elmls = {} },
  formatters = { elm = { "elm_format" } },
})
