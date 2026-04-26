-- lua/lib/lang.lua
-- Helper for the recurring lang/*.lua shape: gate on an executable, register
-- mason tools, install treesitter parsers, configure LSP servers (capabilities
-- merged automatically), wire conform formatters and nvim-lint linters,
-- register neotest adapters, and return the plugin specs. Anything outside
-- this shape (DAP adapters, jdtls bootstrap, rustaceanvim, etc.) keeps using
-- the imperative helpers (Lib.lsp.on_attach, Lib.plugin.on_load, ...) before
-- or after the setup call.

local M = {}

-- spec fields, all optional:
--   cmd        string                    — executable name; if missing, return early
--   mason      {string,...}              — mason-tool-installer ensure_installed entries
--   parsers    {string,...}              — nvim-treesitter parsers to install
--   servers    { [name] = config }       — vim.lsp.config + Lib.lsp.enable per server.
--                                          Capabilities are deep-merged with
--                                          Lib.lsp.capabilities(). Two helper-only
--                                          fields, stripped before vim.lsp.config:
--                                            _enable    { cmd = "binary" } passed to
--                                                       Lib.lsp.enable as a hint for
--                                                       servers with function `cmd`.
--                                            _on_attach function(args, client) — fired
--                                                       on LspAttach, pre-filtered to
--                                                       this server name; client is
--                                                       resolved from args for you.
--   formatters { [filetype] = {tool,...} } — conform formatters_by_ft entries
--   formatters_setup function(conform)   — extra conform setup (custom formatter
--                                          definitions, etc.) ran inside the
--                                          conform.nvim on_load
--   linters    { [filetype] = {tool,...} } — nvim-lint linters_by_ft entries
--   neotest    { [name] = factory_fn }   — Lib.neotest.add(name, factory) per entry
--   plugins    {plugin_spec,...}         — pack specs returned to caller
function M.setup(spec)
  if spec.cmd and vim.fn.executable(spec.cmd) == 0 then
    return {}
  end

  if spec.mason then
    for _, tool in ipairs(spec.mason) do
      Lib.mason.add(tool)
    end
  end

  if spec.neotest then
    for name, factory in pairs(spec.neotest) do
      Lib.neotest.add(name, factory)
    end
  end

  if spec.parsers and #spec.parsers > 0 then
    local parsers = spec.parsers
    Lib.plugin.on_load("nvim-treesitter", function()
      require("nvim-treesitter").install(parsers)
    end)
  end

  if spec.servers then
    local servers = spec.servers
    Lib.plugin.on_load("nvim-lspconfig", function()
      for name, cfg in pairs(servers) do
        cfg = cfg or {}
        local enable_opts = cfg._enable
        local on_attach_cb = cfg._on_attach
        local merged = vim.tbl_deep_extend(
          "force",
          { capabilities = Lib.lsp.capabilities() },
          cfg
        )
        merged._enable = nil
        merged._on_attach = nil
        vim.lsp.config(name, merged)
        Lib.lsp.enable(name, enable_opts)

        if on_attach_cb then
          local server_name = name
          Lib.lsp.on_attach(function(args)
            local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
            if not client or client.name ~= server_name then return end
            on_attach_cb(args, client)
          end)
        end
      end
    end)
  end

  if spec.formatters or spec.formatters_setup then
    local formatters = spec.formatters
    local setup_fn = spec.formatters_setup
    Lib.plugin.on_load("conform.nvim", function()
      local conform = require("conform")
      if formatters then
        for ft, tools in pairs(formatters) do
          conform.formatters_by_ft[ft] = tools
        end
      end
      if setup_fn then setup_fn(conform) end
    end)
  end

  if spec.linters then
    local linters = spec.linters
    Lib.plugin.on_load("nvim-lint", function()
      local lint = require("lint")
      for ft, tools in pairs(linters) do
        lint.linters_by_ft[ft] = tools
      end
    end)
  end

  return spec.plugins or {}
end

return M
