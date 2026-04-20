-- lua/config/plugins/lang/svelte.lua
if vim.fn.executable("node") == 0 then return {} end

Lib.mason.add("svelte-language-server")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "svelte" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  local caps = Lib.lsp.capabilities()
  caps = vim.tbl_deep_extend("force", caps, {
    workspace = {
      didChangeWatchedFiles = vim.fn.has("nvim-0.10") == 0 and { dynamicRegistration = true } or nil,
    },
  })
  vim.lsp.config("svelte", { capabilities = caps })
  vim.lsp.enable("svelte")
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.svelte = { "prettier" }
end)

-- Organize-imports keymap on svelte buffers
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
    if not client or client.name ~= "svelte" then return end
    vim.keymap.set("n", "<Leader>co", function()
      vim.lsp.buf.code_action({
        apply = true,
        context = { only = { "source.organizeImports" }, diagnostics = {} },
      })
    end, { buffer = args.buf, desc = "Organize Imports" })
  end,
})

-- Note: old config coordinated svelte with vtsls via `typescript-svelte-plugin`
-- for TS intelligence inside .svelte files. Skipped per M4 simplification.

return {}
