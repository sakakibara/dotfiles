-- lua/config/plugins/lang/gotmpl.lua
-- Treesitter parser for go templates. Used by lib.chezmoi to inject
-- gotmpl into {{...}} regions of *.tmpl files; also gives standalone
-- *.gotmpl files proper highlighting.
return Lib.lang.setup({
  parsers = { "gotmpl" },
})
