-- lua/config/plugins/lang/angular.lua
if vim.fn.executable("node") == 0 then return {} end

Lib.mason.add("angular-language-server")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "angular", "scss" })
end)

-- Force angular treesitter parser on component HTML files (ft normally 'html')
vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
  pattern = { "*.component.html", "*.container.html" },
  callback = function()
    pcall(vim.treesitter.start, nil, "angular")
  end,
})

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("angularls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("angularls")
end)

-- Disable angularls rename — it clashes with the TS server's rename capability.
Lib.lsp.on_attach(function(args)
  local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
  if not client or client.name ~= "angularls" then return end
  client.server_capabilities.renameProvider = false
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.htmlangular = { "prettier" }
end)

-- Note: old config also wired `@angular/language-server` as a vtsls global
-- plugin so Angular-aware completions would fire inside .ts files. Skipped
-- per M4 simplification — angularls alone handles Angular templates.

return {}
