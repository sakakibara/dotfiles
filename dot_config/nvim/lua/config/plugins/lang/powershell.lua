-- lua/config/plugins/lang/powershell.lua
if vim.fn.executable("pwsh") == 0 then return {} end

-- powershell-editor-services is a bundled DLL set launched via pwsh; mason
-- installs it under its packages dir, which is what `bundle_path` points to below.
Lib.mason.add("powershell-editor-services")

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("powershell_es", {
    capabilities = Lib.lsp.capabilities(),
    bundle_path = vim.fn.stdpath("data") .. "/mason/packages/powershell-editor-services",
  })
  Lib.lsp.enable("powershell_es")
end)

return {
  { "sakakibara/vim-ps1", name = "vim-ps1", ft = "ps1" },
}
