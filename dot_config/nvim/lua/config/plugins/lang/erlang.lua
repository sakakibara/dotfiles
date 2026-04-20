-- lua/config/plugins/lang/erlang.lua
if vim.fn.executable("erl") == 0 then return {} end

Lib.mason.add("erlang-ls")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "erlang" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("erlangls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("erlangls")
end)

return {}
