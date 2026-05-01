-- lua/config/plugins/lsp.lua
return {
  {
    "neovim/nvim-lspconfig",
    name = "nvim-lspconfig",
    event = "LazyFile",
    dependencies = { "lazydev.nvim" },
    init = function()
      Lib.mason.add("lua-language-server", { ft = "lua" })
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
      Lib.lsp.enable("lua_ls")
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
    config = function(_, opts)
      require("mason").setup(opts)

      -- Install tools on first FileType match. Tracks which fts have
      -- been seen so we only refresh / scan the registry once per ft
      -- (mason's own install is itself idempotent).
      local function install_missing(names)
        if not names or #names == 0 then return end
        local registry = require("mason-registry")
        registry.refresh(function()
          for _, name in ipairs(names) do
            local ok, pkg = pcall(registry.get_package, name)
            if ok and pkg and not pkg:is_installed() then
              pkg:install()
            end
          end
        end)
      end

      local seen_ft = {}
      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("Lib.mason.on_demand", { clear = true }),
        callback = function(args)
          local ft = vim.bo[args.buf].filetype
          if ft == "" or seen_ft[ft] then return end
          seen_ft[ft] = true
          local tools = Lib.mason.list_for_ft(ft)
          if #tools > 0 then install_missing(tools) end
        end,
      })
    end,
  },
}
