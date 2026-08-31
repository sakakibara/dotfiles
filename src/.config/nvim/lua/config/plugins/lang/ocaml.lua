return Lib.lang.setup({
  cmd = "ocaml",
  ft = { "ocaml", "ocamlinterface", "ocamllex", "menhir", "reason", "dune" },
  mason = { "ocamllsp" },
  parsers = { "ocaml", "ocaml_interface", "ocamllex", "menhir" },
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
