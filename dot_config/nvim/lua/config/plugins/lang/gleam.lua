-- lua/config/plugins/lang/gleam.lua
if vim.fn.executable("gleam") == 0 then return {} end

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "gleam" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("gleam", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("gleam")
end)

return {}
