-- lua/config/plugins/lsp.lua
return {
  {
    "neovim/nvim-lspconfig",
    name = "nvim-lspconfig",
    event = "LazyFile",
    dependencies = { "lazydev.nvim" },
    init = function()
      Lib.mason.add("lua-language-server")
    end,
    config = function()
      local caps = Lib.lsp.capabilities()

      -- lua_ls
      vim.lsp.config("lua_ls", {
        capabilities = caps,
        settings = {
          Lua = {
            workspace = { checkThirdParty = false },
            codeLens = { enable = true },
            completion = { callSnippet = "Replace" },
            doc = { privateName = { "^_" } },
            hint = {
              enable = true,
              setType = false,
              paramType = true,
              paramName = "Disable",
              semicolon = "Disable",
              arrayIndex = "Disable",
            },
          },
        },
      })
      vim.lsp.enable("lua_ls")
    end,
  },

  {
    "folke/lazydev.nvim",
    name = "lazydev.nvim",
    ft = "lua",
    opts = {
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
        { path = "snacks.nvim", words = { "Snacks" } },
        { path = "lazy.nvim",    words = { "LazyVim" } },
      },
    },
  },

  {
    "mason-org/mason.nvim",
    name = "mason.nvim",
    event = "VeryLazy",
    opts = {
      ui = { border = "rounded" },
    },
  },

  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    name = "mason-tool-installer.nvim",
    event = "VeryLazy",
    dependencies = { "mason.nvim" },
    config = function()
      require("mason-tool-installer").setup({
        ensure_installed = Lib.mason.list(),
        auto_update = false,
        run_on_start = true,
        start_delay = 3000,
        debounce_hours = 5,
      })
    end,
  },
}
