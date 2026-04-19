-- lua/config/plugins/lang/prisma.lua
if vim.fn.executable("node") == 0 then return {} end

Lib.mason.add("prisma-language-server")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "prisma" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("prismals", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("prismals")
end)

return {}
