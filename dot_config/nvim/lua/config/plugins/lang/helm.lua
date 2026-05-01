-- lua/config/plugins/lang/helm.lua

-- Stop yamlls on helm-template buffers: yamlls and helm_ls both claim yaml
-- files, and yamlls emits noisy diagnostics on helm templates. Registered
-- unconditionally — the handler self-filters by client name + filetype, so
-- it's safe even when helm itself isn't installed.
Lib.lsp.on_attach(function(args)
  local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
  if not client or client.name ~= "yamlls" then return end
  if vim.bo[args.buf].filetype == "helm" then
    vim.schedule(function()
      pcall(vim.lsp.stop_client, client.id, true)
    end)
  end
end)

return Lib.lang.setup({
  cmd = "helm",
  mason = { "helm-ls" },
  parsers = { "helm" },
  servers = { helm_ls = {} },
  plugins = {
    { "towolf/vim-helm", name = "vim-helm", ft = "helm" },
  },
})
