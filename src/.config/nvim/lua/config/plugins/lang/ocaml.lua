return Lib.lang.setup({
  cmd = "ocaml",
  ft = { "ocaml", "dune" },
  no_parser = { "dune" },
  mason = { "ocaml-lsp" },
  parsers = { "ocaml" },
  servers = {
    ocamllsp = {},
  },
})
