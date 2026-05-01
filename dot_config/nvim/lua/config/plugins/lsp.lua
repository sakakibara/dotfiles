-- lua/config/plugins/lsp.lua
return {
  {
    "neovim/nvim-lspconfig",
    name = "nvim-lspconfig",
    event = "LazyFile",
    -- lazydev.nvim is NOT a dependency: nvim-lspconfig is the
    -- LazyFile gate (any file), which would force lazydev to load on
    -- every file type. lazydev has its own `ft = "lua"` trigger; we
    -- pull it in explicitly only when lua_ls actually attaches via
    -- the LspAttach hook below.
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
            -- Baseline runtime + library so `vim` is defined from the
            -- initial lua_ls attach, before lazydev has a chance to
            -- inject via workspace/didChangeConfiguration. lazydev
            -- still runs and adds plugin-specific paths
            -- (snacks.nvim, lazy.nvim) on top.
            runtime = { version = "LuaJIT" },
            workspace = {
              checkThirdParty = false,
              -- All rtp paths so lua_ls finds nvim's API stubs
              -- (lua/vim/_meta — defines `vim`, `vim.api`, `vim.fn`,
              -- etc.) plus every loaded plugin's lua/. lazydev's
              -- workspace/didChangeConfiguration push extends this
              -- with plugin-specific paths from the lazydev opts.
              library = vim.api.nvim_get_runtime_file("", true),
            },
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

      -- Populate lazydev's per-workspace settings + install its
      -- workspace/configuration handler synchronously on lua_ls
      -- attach, BEFORE lua_ls sends its first workspace/configuration
      -- request.
      --
      -- The race we're closing: lazydev replaces the
      -- workspace/configuration handler to return ws.settings (a
      -- per-workspace table built from client.settings + lazydev's
      -- library injections). ws.settings starts empty {}; it's only
      -- populated by workspace:update(). lazydev's own LspAttach hook
      -- triggers update via a debounced (uv timer + vim.schedule_wrap)
      -- path, which runs at least one tick after attach. lua_ls
      -- typically sends workspace/configuration BEFORE that tick →
      -- handler returns empty Lua section → lua_ls processes the
      -- buffer with no library → "vim is not defined" diagnostics.
      --
      -- We bypass the debounce by calling workspace:update + Lsp.attach
      -- (the handler installer) directly. Synchronous, idempotent.
      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if not client or client.name ~= "lua_ls" then return end
          local ok_ws, Workspace = pcall(require, "lazydev.workspace")
          local ok_lsp, LzdLsp   = pcall(require, "lazydev.lsp")
          if not (ok_ws and ok_lsp) then return end
          -- Populate every workspace lua_ls might query in its
          -- workspace/configuration request:
          --   - Workspace.get(client, ws.name) for each workspace_folder
          --     (lua_ls passes scopeUri = folder.uri)
          --   - Workspace.single(client) as the no-folders fallback
          -- Mirrors lazydev's own buf.update folder enumeration.
          if client.workspace_folders and #client.workspace_folders > 0 then
            for _, wsf in ipairs(client.workspace_folders) do
              Workspace.get(client, wsf.name):update()
            end
          else
            Workspace.single(client):update()
          end
          LzdLsp.attach(client)
          -- Send didChangeConfiguration explicitly: covers the case
          -- where lua_ls already issued its initial config request
          -- and got empty settings before our handler installed.
          -- lua_ls will re-request configuration on this notification
          -- and now get the populated ws.settings.
          LzdLsp.update(client)
        end,
      })
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
              pkg:install()
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
