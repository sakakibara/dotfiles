vim.filetype.add({
  extension = {
    pu = "plantuml",
    uml = "plantuml",
    plantuml = "plantuml",
    puml = "plantuml",
    iuml = "plantuml",
  },
})

return Lib.lang.setup({
  plugins = {
    { "aklt/plantuml-syntax", ft = "plantuml" },
  },
})
