-- Note: old config coordinated svelte with vtsls via `typescript-svelte-plugin`
-- for TS intelligence inside .svelte files. Skipped per M4 simplification.
return Lib.lang.setup({
  cmd = "node",
  mason = { "svelte-language-server" },
  parsers = { "svelte" },
  servers = {
    svelte = {
      capabilities = {
        workspace = {
          didChangeWatchedFiles = vim.fn.has("nvim-0.10") == 0 and { dynamicRegistration = true } or nil,
        },
      },
      on_attach = function(args, _)
        vim.keymap.set("n", "<Leader>co", function()
          vim.lsp.buf.code_action({
            apply = true,
            context = { only = { "source.organizeImports" }, diagnostics = {} },
          })
        end, { buffer = args.buf, desc = "Organize imports" })
      end,
    },
  },
  formatters = { svelte = { "prettier" } },
})
