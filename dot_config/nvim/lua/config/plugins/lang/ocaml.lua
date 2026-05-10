return Lib.lang.setup({
  cmd = "ocaml",
  mason = { "ocamllsp" },
  parsers = { "ocaml" },
  servers = {
    ocamllsp = {
      filetypes = {
        "ocaml",
        "ocaml.menhir",
        "ocaml.interface",
        "ocaml.ocamllex",
        "reason",
        "dune",
      },
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
