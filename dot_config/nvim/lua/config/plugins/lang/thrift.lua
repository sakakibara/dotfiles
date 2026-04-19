-- lua/config/plugins/lang/thrift.lua
if vim.fn.executable("thrift") == 0 then return {} end

-- thriftls is not in the mason registry; install manually if needed.

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "thrift" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("thriftls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("thriftls")
end)

return {}
