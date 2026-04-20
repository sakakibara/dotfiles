-- lua/config/plugins/lang/toml.lua
Lib.mason.add("taplo")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "toml" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("taplo", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("taplo")
end)

return {}
