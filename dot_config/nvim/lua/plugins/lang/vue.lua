local uplugin = require("util.plugin")

return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "vue" })
      end
    end,
  },

  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      local vue_typescript_plugin = require("mason-registry").get_package("vue-language-server"):get_install_path()
        .. "/node_modules/@vue/language-server"
        .. "/node_modules/@vue/typescript-plugin"

      opts.servers = vim.tbl_deep_extend("force", opts.servers, {
        volar = {},
        vtsls = {
          filetypes = {
            "javascript",
            "javascriptreact",
            "javascript.jsx",
            "typescript",
            "typescriptreact",
            "typescript.tsx",
            "vue",
          },
        },
      })
      uplugin.extend(opts.servers.vtsls, "settings.vtsls.tsserver.globalPlugins", {
        name = "@vue/typescript-plugin",
        location = vue_typescript_plugin,
        languages = { "vue" },
        configNamespace = "typescript",
      })
    end,
  },
}
