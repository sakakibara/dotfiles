return Lib.lang.setup({
  cmd = "helm",
  ft = "helm",
  mason = { "helm-ls" },
  parsers = { "helm", "gotmpl" },
  servers = { helm_ls = {} },
  plugins = {
    { "towolf/vim-helm", lazy = false },
  },
})
