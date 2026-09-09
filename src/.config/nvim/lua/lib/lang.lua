-- Helper for the recurring lang/*.lua shape: gate on an executable, register
-- mason tools, install treesitter parsers, configure LSP servers (capabilities
-- merged automatically), wire conform formatters and nvim-lint linters,
-- register neotest adapters, and return the plugin specs. Anything outside
-- this shape (DAP adapters, jdtls bootstrap, rustaceanvim, etc.) keeps using
-- the imperative helpers (Lib.lsp.on_attach, Lib.plugin.on_load, ...) before
-- or after the setup call.

local M = {}

-- spec fields; ft is required when the spec declares mason tools or
-- parsers, the rest are optional:
--   ft         string|{string,...}       -- filetypes this spec registers for
--   cmd        string                    -- executable name; if missing, return early
--   mason      {string,...}              -- mason packages installed on the first buffer of ft
--   parsers    {string,...}              -- nvim-treesitter parsers to install
--   no_parser  {string,...}              -- fts of the spec none of those parsers serves
--   parsers_setup function()             -- extra ts work ran in nvim-treesitter's on_load
--   servers    { [name] = config }       -- vim.lsp.config + Lib.lsp.enable per server.
--                                          Capabilities are deep-merged with
--                                          Lib.lsp.capabilities(). config may also be
--                                          a function() that returns the table, for a
--                                          config that reads modules nvim-lspconfig
--                                          loads (lspconfig.configs.*, schemastore).
--                                          Three helper-only fields, stripped before
--                                          vim.lsp.config:
--                                            binary     "ruby-lsp" -- the binary name
--                                                       Lib.lsp.enable probes for a
--                                                       server with a function `cmd`
--                                            available  function() -> bool -- the
--                                                       readiness check itself, for a
--                                                       launcher that is always present
--                                                       (powershell_es)
--                                            on_attach  function(args, client) -- fired
--                                                       on LspAttach for this server;
--                                                       client is resolved from args
--   formatters { [filetype] = {tool,...} } -- conform formatters_by_ft entries
--   formatters_setup function(conform)   -- extra conform setup (custom formatters)
--                                          ran in conform.nvim's on_load
--   linters    { [filetype] = {tool,...} } -- nvim-lint linters_by_ft entries
--   neotest    { [name] = factory_fn }   -- Lib.neotest.add(name, factory) per entry
--   plugins    {plugin_spec,...}         -- pack specs returned to caller
function M.setup(spec)
  if (spec.mason or (spec.parsers and #spec.parsers > 0)) and not spec.ft then
    error("Lib.lang.setup: ft is required when a spec declares mason tools or parsers")
  end

  if (spec.parsers and #spec.parsers > 0) or spec.parsers_setup then
    -- Register parsers in the per-ft on-demand registry. The treesitter
    -- spec's FileType autocmd reads the registry and installs the
    -- parsers the first time a buffer of that ft opens. parsers_setup
    -- is still fired at nvim-treesitter load time (rare; for parsers
    -- that need extra wiring beyond install).
    if spec.parsers and #spec.parsers > 0 then
      local fts = type(spec.ft) == "table" and spec.ft or { spec.ft }
      if spec.no_parser then
        fts = vim.tbl_filter(function(ft) return not vim.tbl_contains(spec.no_parser, ft) end, fts)
      end
      for _, parser in ipairs(spec.parsers) do
        Lib.parsers.add(parser, { ft = fts })
      end
    end
    if spec.parsers_setup then
      Lib.plugin.on_load("nvim-treesitter", spec.parsers_setup)
    end
  end

  if spec.cmd and vim.fn.executable(spec.cmd) == 0 then
    return {}
  end

  if spec.mason then
    for _, tool in ipairs(spec.mason) do
      Lib.mason.add(tool, { ft = spec.ft })
    end
  end

  if spec.neotest then
    for name, factory in pairs(spec.neotest) do
      Lib.neotest.add(name, factory)
    end
  end

  if spec.servers then
    local servers = spec.servers
    Lib.plugin.on_load("nvim-lspconfig", function()
      for name, cfg in pairs(servers) do
        if type(cfg) == "function" then cfg = cfg() end
        cfg = cfg or {}
        local binary = cfg.binary
        local available = cfg.available
        local on_attach_cb = cfg.on_attach
        local merged = vim.tbl_deep_extend(
          "force",
          { capabilities = Lib.lsp.capabilities() },
          cfg
        )
        merged.binary = nil
        merged.available = nil
        merged.on_attach = nil
        vim.lsp.config(name, merged)
        Lib.lsp.enable(name, (binary or available) and { cmd = binary, available = available } or nil)

        if on_attach_cb then
          Lib.lsp.on_attach(name, on_attach_cb)
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
