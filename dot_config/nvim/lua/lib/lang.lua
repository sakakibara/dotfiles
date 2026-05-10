-- Helper for the recurring lang/*.lua shape: gate on an executable, register
-- mason tools, install treesitter parsers, configure LSP servers (capabilities
-- merged automatically), wire conform formatters and nvim-lint linters,
-- register neotest adapters, and return the plugin specs. Anything outside
-- this shape (DAP adapters, jdtls bootstrap, rustaceanvim, etc.) keeps using
-- the imperative helpers (Lib.lsp.on_attach, Lib.plugin.on_load, ...) before
-- or after the setup call.

local M = {}

-- Default ft for mason/parsers when the caller doesn't pass spec.ft.
--
-- Two cases handled:
--   1. The lang/<x>.lua chunk's frame is on the stack (no Lua tail-call
--      eliminated it). Match its source path directly.
--   2. The chunk used `return Lib.lang.setup(...)`, a tail call — Lua
--      drops the chunk's frame and we never see lang/<x>.lua. Walk
--      further up to `config/plugins.lua`'s loader (which has work
--      after the require, so its frame survives) and read its `name`
--      local — that's the module path being loaded ("lang.<x>").
local function caller_ft()
  for level = 1, 16 do
    local info = debug.getinfo(level, "S")
    if not info then break end
    local src = info.source or ""

    local direct = src:match("/config/plugins/lang/([%w_]+)%.lua$")
    if direct then return direct end

    if src:match("/config/plugins%.lua$") then
      for i = 1, 10 do
        local lname, lvalue = debug.getlocal(level, i)
        if not lname then break end
        if lname == "name" and type(lvalue) == "string" then
          local ft = lvalue:match("lang%.([%w_]+)$")
          if ft then return ft end
        end
      end
    end
  end
  return nil
end

-- spec fields, all optional:
--   cmd        string                    — executable name; if missing, return early
--   mason      {string,...}              — mason-tool-installer ensure_installed entries
--   parsers    {string,...}              — nvim-treesitter parsers to install
--   parsers_setup function()             — extra ts work (e.g. registering a custom
--                                          parser, disabling a highlight) ran inside
--                                          the nvim-treesitter on_load BEFORE install
--   servers    { [name] = config }       — vim.lsp.config + Lib.lsp.enable per server.
--                                          Capabilities are deep-merged with
--                                          Lib.lsp.capabilities(). config may also be
--                                          a function() that returns the table — used
--                                          when the config needs to look at modules
--                                          loaded by nvim-lspconfig itself
--                                          (lspconfig.configs.*, schemastore, ...).
--                                          Two helper-only fields, stripped before
--                                          vim.lsp.config:
--                                            binary     "ruby-lsp" — explicit binary
--                                                       name passed to Lib.lsp.enable
--                                                       as a hint for servers with
--                                                       function `cmd` (jsonls, vtsls,
--                                                       ruby_lsp, yamlls, angularls).
--                                            on_attach  function(args, client) — fired
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
    -- Determine on-demand ft for mason tools. Priority:
    --   1. spec.ft           — explicit override (e.g. multi-ft langs)
    --   2. caller's filename — lang/<x>.lua → "<x>"  (covers most specs)
    -- A ft must be resolvable; mason install is on-demand only.
    local ft = spec.ft or caller_ft()
    if not ft then
      error("Lib.lang.setup: cannot determine ft for mason tools (set spec.ft explicitly)")
    end
    for _, tool in ipairs(spec.mason) do
      Lib.mason.add(tool, { ft = ft })
    end
  end

  if spec.neotest then
    for name, factory in pairs(spec.neotest) do
      Lib.neotest.add(name, factory)
    end
  end

  if (spec.parsers and #spec.parsers > 0) or spec.parsers_setup then
    -- Register parsers in the per-ft on-demand registry. The treesitter
    -- spec's FileType autocmd reads the registry and installs the
    -- parsers the first time a buffer of that ft opens. parsers_setup
    -- is still fired at nvim-treesitter load time (rare; for parsers
    -- that need extra wiring beyond install).
    if spec.parsers and #spec.parsers > 0 then
      -- Use the same ft we resolved for mason.
      local parser_ft = spec.ft or caller_ft()
      if not parser_ft then
        error("Lib.lang.setup: cannot determine ft for parsers (set spec.ft)")
      end
      for _, parser in ipairs(spec.parsers) do
        Lib.parsers.add(parser, { ft = parser_ft })
      end
    end
    if spec.parsers_setup then
      Lib.plugin.on_load("nvim-treesitter", spec.parsers_setup)
    end
  end

  if spec.servers then
    local servers = spec.servers
    Lib.plugin.on_load("nvim-lspconfig", function()
      for name, cfg in pairs(servers) do
        if type(cfg) == "function" then cfg = cfg() end
        cfg = cfg or {}
        local binary = cfg.binary
        local on_attach_cb = cfg.on_attach
        local merged = vim.tbl_deep_extend(
          "force",
          { capabilities = Lib.lsp.capabilities() },
          cfg
        )
        merged.binary = nil
        merged.on_attach = nil
        vim.lsp.config(name, merged)
        Lib.lsp.enable(name, binary and { cmd = binary } or nil)

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
