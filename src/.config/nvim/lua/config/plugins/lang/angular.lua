vim.treesitter.language.register("angular", { "htmlangular" })

vim.filetype.add({
  pattern = {
    [".*%.component%.html"] = "htmlangular",
    [".*%.container%.html"] = "htmlangular",
  },
})

-- angularls serves the templates. TypeScript files get no Angular-aware
-- completions: that needs `@angular/language-server` wired into vtsls as a
-- global plugin, which this config does not do.
return Lib.lang.setup({
  cmd = "node",
  ft = "htmlangular",
  mason = { "angular-language-server", "prettier" },
  parsers = { "angular", "scss" },
  servers = {
    angularls = {
      binary = "ngserver",  -- function cmd in lspconfig
      -- Disable angularls rename -- it clashes with the TS server's rename capability.
      on_attach = function(_, client)
        client.server_capabilities.renameProvider = false
      end,
    },
  },
  formatters = { htmlangular = { "prettier" } },
})
