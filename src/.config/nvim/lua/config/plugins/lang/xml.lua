vim.treesitter.language.register("xml", { "svg", "xsd", "xslt" })

return Lib.lang.setup({
  ft = { "xml", "xsd", "xslt", "svg" },
  parsers = { "xml" },
})
