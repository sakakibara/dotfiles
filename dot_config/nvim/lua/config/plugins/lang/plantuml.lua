-- lua/config/plugins/lang/plantuml.lua
return Lib.lang.setup({
  plugins = {
    { "aklt/plantuml-syntax", name = "plantuml-syntax", ft = "plantuml" },
  },
})
