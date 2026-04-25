-- lua/lib/lang.lua
-- Helper for the recurring lang/*.lua shape: gate on an executable, register
-- mason tools, install treesitter parsers, configure LSP servers (capabilities
-- merged automatically), wire conform formatters, register neotest adapters,
-- and return the plugin specs. Anything outside this shape (custom on_attach
-- hooks, dap adapters, jdtls bootstrap, etc.) keeps using the imperative
-- helpers (Lib.lsp.on_attach, Lib.plugin.on_load, ...) before or after the
-- setup call.

local M = {}

-- spec fields, all optional:
--   cmd        string                    — executable name; if missing, return early
--   mason      {string,...}              — mason-tool-installer ensure_installed entries
--   parsers    {string,...}              — nvim-treesitter parsers to install
--   servers    { [name] = config_table } — vim.lsp.config + Lib.lsp.enable per server;
--                                          capabilities are deep-merged with Lib.lsp.capabilities()
--   formatters { [filetype] = {tool,...} } — conform formatters_by_ft entries
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
        local merged = vim.tbl_deep_extend(
          "force",
          { capabilities = Lib.lsp.capabilities() },
          cfg or {}
        )
        vim.lsp.config(name, merged)
        Lib.lsp.enable(name)
      end
    end)
  end

  if spec.formatters then
    local formatters = spec.formatters
    Lib.plugin.on_load("conform.nvim", function()
      local conform = require("conform")
      for ft, tools in pairs(formatters) do
        conform.formatters_by_ft[ft] = tools
      end
    end)
  end

  return spec.plugins or {}
end

return M
