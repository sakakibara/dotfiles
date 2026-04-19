-- lua/config/plugins/lang/helm.lua
if vim.fn.executable("helm") == 0 then return {} end

Lib.mason.add("helm-ls")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "helm" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("helm_ls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("helm_ls")
end)

-- Stop yamlls on helm-template buffers: yamlls and helm_ls both claim yaml
-- files, and yamlls emits noisy diagnostics on helm templates. The old config
-- did this via a setup/on_attach callback; we replicate via a shared on_attach.
Lib.lsp.on_attach(function(args)
  local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
  if not client or client.name ~= "yamlls" then return end
  if vim.bo[args.buf].filetype == "helm" then
    vim.schedule(function()
      pcall(vim.lsp.stop_client, client.id, true)
    end)
  end
end)

return {
  { "towolf/vim-helm", name = "vim-helm", ft = "helm" },
}
