return Lib.lang.setup({
  cmd = "ocaml",
  ft = { "ocaml", "dune" },
  mason = { "ocaml-lsp" },
  parsers = { "ocaml" },
  servers = {
    ocamllsp = {
      filetypes = { "ocaml", "dune" },
      root_markers = {
        "*.opam",
        "esy.json",
        "package.json",
        "dune-project",
        "dune-workspace",
        ".git",
      },
    },
  },
})
