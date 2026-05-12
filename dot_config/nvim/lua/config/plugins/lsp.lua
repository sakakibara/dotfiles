return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile", "BufWritePre" },
    -- lazydev.nvim is NOT a dependency: nvim-lspconfig is the
    -- LazyFile gate (any file), which would force lazydev to load on
    -- every file type. lazydev has its own `ft = "lua"` trigger; we
    -- pull it in explicitly only when lua_ls actually attaches via
    -- the LspAttach hook below.
    init = function()
      Lib.mason.add("lua-language-server", { ft = "lua" })
    end,
    config = function()
      vim.lsp.config("lua_ls", {
        capabilities = Lib.lsp.capabilities(),
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
    event = "VeryLazy",
    opts = {
      ui = { border = "rounded" },
    },
    config = function(_, opts)
      require("mason").setup(opts)

      -- After every install finishes, fire MasonToolsUpdateCompleted
      -- so Lib.lsp's pending-enable queue drains and calls
      -- vim.lsp.enable(name) for any LSP whose binary is now present.
      -- vim.lsp.enable internally does doautoall('nvim.lsp.enable
      -- FileType') for all loaded buffers, which calls the callback
      -- that spawns the server and attaches to matching buffers.
      --
      -- The autocmd dispatch is scheduled (not immediate) so the
      -- mason install hook returns first; nvim then processes the
      -- User event on the next tick, well after the package is
      -- registered on PATH.
      local function drain_pending_lsps()
        vim.schedule(function()
          pcall(vim.api.nvim_exec_autocmds, "User", {
            pattern  = "MasonToolsUpdateCompleted",
            modeline = false,
          })
        end)
      end

      -- Install tools on first FileType match. After all installs in
      -- the batch complete, drain pending LSPs once.
      local function install_missing(names)
        if not names or #names == 0 then return end
        local registry = require("mason-registry")
        registry.refresh(function()
          local pending = 0
          local function on_install_done()
            if pending == 0 then drain_pending_lsps() end
          end
          for _, name in ipairs(names) do
            local ok, pkg = pcall(registry.get_package, name)
            if ok and pkg and not pkg:is_installed() then
              pending = pending + 1
              local handler
              handler = function()
                pending = pending - 1
                pcall(pkg.off, pkg, "install:success", handler)
                pcall(pkg.off, pkg, "install:failed",  handler)
                on_install_done()
              end
              pcall(pkg.on, pkg, "install:success", handler)
              pcall(pkg.on, pkg, "install:failed",  handler)
              -- Skip the install() call when another trigger has already
              -- kicked it off (e.g. a tool shared between two filetypes).
              -- pkg:install() asserts on re-entry, so we must guard. Our
              -- handler still fires when the in-flight install completes.
              if not pkg:is_installing() then
                pkg:install()
              end
            end
          end
          -- If everything was already installed, still drain once
          -- to flush any earlier deferrals.
          on_install_done()
        end)
      end

      local seen_ft = {}
      local function trigger_for_ft(ft)
        if ft == "" or seen_ft[ft] then return end
        seen_ft[ft] = true
        local tools = Lib.mason.list_for_ft(ft)
        if #tools > 0 then install_missing(tools, ft) end
      end

      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("Lib.mason.on_demand", { clear = true }),
        callback = function(args) trigger_for_ft(vim.bo[args.buf].filetype) end,
      })

      -- Catch buffers whose FileType fired BEFORE our autocmd was
      -- registered — happens on `nvim foo.lua` cold start, where the
      -- arg-file's FileType is dispatched before pack.setup wires up
      -- mason. Without this scan, that buffer's LSP/mason install
      -- never triggers until the user does something that re-fires
      -- FileType (`:e`, new buffer, etc.).
      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) then
          trigger_for_ft(vim.bo[buf].filetype)
        end
      end
    end,
  },
}
