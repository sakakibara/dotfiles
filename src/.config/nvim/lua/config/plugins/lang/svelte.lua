-- svelte-language-server alone serves .svelte files; TS intelligence inside
-- them would need `typescript-svelte-plugin` coordinated with vtsls.
return Lib.lang.setup({
  cmd = "node",
  ft = "svelte",
  mason = { "svelte-language-server", "prettier" },
  parsers = { "svelte", "html", "css", "javascript" },
  servers = {
    svelte = {
      binary = "svelteserver",
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
