-- Note: old config also wired `@angular/language-server` as a vtsls global
-- plugin so Angular-aware completions would fire inside .ts files. Skipped
-- per M4 simplification — angularls alone handles Angular templates.
return Lib.lang.setup({
  cmd = "node",
  mason = { "angular-language-server" },
  parsers = { "angular", "scss" },
  parsers_setup = function()
    -- Force angular treesitter parser on component HTML files (ft normally 'html')
    vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
      pattern = { "*.component.html", "*.container.html" },
      callback = function()
        pcall(vim.treesitter.start, nil, "angular")
      end,
    })
  end,
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
