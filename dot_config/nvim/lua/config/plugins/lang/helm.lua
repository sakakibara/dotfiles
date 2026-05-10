-- Stop yamlls on helm-template buffers: yamlls and helm_ls both claim yaml
-- files, and yamlls emits noisy diagnostics on helm templates. Registered
-- unconditionally; safe even when helm itself isn't installed because the
-- name filter only fires on actual yamlls attaches.
Lib.lsp.on_attach("yamlls", function(args, client)
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
    { "towolf/vim-helm", ft = "helm" },
  },
})
