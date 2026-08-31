vim.filetype.add({
  pattern = {
    [".*%.component%.html"] = "htmlangular",
    [".*%.container%.html"] = "htmlangular",
  },
})

-- Note: old config also wired `@angular/language-server` as a vtsls global
-- plugin so Angular-aware completions would fire inside .ts files. Skipped
-- per M4 simplification — angularls alone handles Angular templates.
return Lib.lang.setup({
  cmd = "node",
  ft = "htmlangular",
  mason = { "angular-language-server", "prettier" },
  parsers = { "angular", "scss" },
  servers = {
    angularls = {
      binary = "ngserver",  -- function cmd in lspconfig
      -- Disable angularls rename — it clashes with the TS server's rename capability.
      on_attach = function(_, client)
        client.server_capabilities.renameProvider = false
      end,
    },
  },
  formatters = { htmlangular = { "prettier" } },
})
