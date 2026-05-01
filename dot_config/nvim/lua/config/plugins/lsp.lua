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

      -- Re-fire FileType for every loaded buffer of `ft` so plugins
      -- that listen on FileType (nvim-lspconfig's vim.lsp.enable, etc.)
      -- get a second chance to attach now that the binary is available.
      -- Their original attempt at the user's first file open ran before
      -- mason finished installing the LSP, so it failed silently.
      local function refire_filetype(ft)
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
          if vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].filetype == ft then
            -- Setting the option to itself triggers FileType handlers,
            -- which is exactly what we want.
            vim.api.nvim_buf_call(buf, function()
              vim.cmd("doautocmd FileType")
            end)
          end
        end
      end

      -- Install tools on first FileType match. Tracks which fts have
      -- been seen so we only refresh / scan the registry once per ft.
      -- After install completes, re-fire FileType so LSPs etc. retry.
      local function install_missing(names, ft)
        if not names or #names == 0 then return end
        local registry = require("mason-registry")
        registry.refresh(function()
          local pending = 0
          local function maybe_refire()
            if pending == 0 then
              vim.schedule(function() refire_filetype(ft) end)
            end
          end
          for _, name in ipairs(names) do
            local ok, pkg = pcall(registry.get_package, name)
            if ok and pkg and not pkg:is_installed() then
              pending = pending + 1
              -- mason packages emit an event when install completes.
              -- pkg:install() returns the package; the "install:success"
              -- and "install:failed" events fire on the package.
              local handler
              handler = function()
                pending = pending - 1
                pcall(pkg.off, pkg, "install:success", handler)
                pcall(pkg.off, pkg, "install:failed",  handler)
                maybe_refire()
              end
              pcall(pkg.on, pkg, "install:success", handler)
              pcall(pkg.on, pkg, "install:failed",  handler)
              pkg:install()
            end
          end
          -- If everything was already installed, still refire once to
          -- recover any earlier missed attaches.
          maybe_refire()
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
          if #tools > 0 then install_missing(tools, ft) end
        end,
      })
    end,
  },
}
