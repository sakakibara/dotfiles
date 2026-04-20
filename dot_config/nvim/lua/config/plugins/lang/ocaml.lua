-- lua/config/plugins/lang/ocaml.lua
if vim.fn.executable("ocaml") == 0 then return {} end

Lib.mason.add("ocamllsp")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "ocaml" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("ocamllsp", {
    capabilities = Lib.lsp.capabilities(),
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
  })
  vim.lsp.enable("ocamllsp")
end)

return {}
