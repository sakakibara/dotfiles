-- powershell-editor-services is a bundled DLL set launched via pwsh; mason
-- installs it under its packages dir, which is what `bundle_path` points to below.
return Lib.lang.setup({
  cmd = "pwsh",
  mason = { "powershell-editor-services" },
  servers = {
    powershell_es = {
      bundle_path = vim.fn.stdpath("data") .. "/mason/packages/powershell-editor-services",
    },
  },
  plugins = {
    { "sakakibara/vim-ps1", ft = "ps1" },
  },
})
