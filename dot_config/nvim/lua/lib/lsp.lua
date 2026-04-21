-- lua/lib/lsp.lua
-- LSP integration: setup, capabilities, buffer keymaps, attach callbacks.
-- Called from config/init.lua at VeryLazy.

local M = {}

local attach_callbacks = {}  -- { fn, fn, ... }

-- Servers we wanted to enable but whose binary wasn't available at the time.
-- Re-checked when mason-tool-installer signals completion.
-- Shape: { [name] = { cmd? = "bin" } } so we can re-check with the same hint.
M._pending_enable = {}
local _pending_hook_installed = false

-- Returns true/false/nil for an LSP's availability:
--   true  = binary present, safe to enable
--   false = binary known-missing, queue for retry
--   nil   = can't determine (no cmd info, no hints) — do nothing safe
local function available(name, opts)
  opts = opts or {}
  -- Explicit binary hint always wins (for function-cmd servers).
  if opts.cmd then
    return vim.fn.executable(opts.cmd) == 1
  end
  local cfg = vim.lsp.config[name]
  if cfg and type(cfg.cmd) == "table" and type(cfg.cmd[1]) == "string" then
    return vim.fn.executable(cfg.cmd[1]) == 1
  end
  -- Function cmd or unknown: without an explicit hint we can't verify.
  -- Return nil so the caller defers rather than optimistically spawning
  -- (which is exactly what produced the "Spawning … failed" spam).
  return nil
end

local function install_pending_hook()
  if _pending_hook_installed then return end
  _pending_hook_installed = true
  vim.api.nvim_create_autocmd("User", {
    pattern = "MasonToolsUpdateCompleted",
    group = vim.api.nvim_create_augroup("Lib.lsp.pending_enable", { clear = true }),
    callback = function()
      for name, entry in pairs(M._pending_enable) do
        if available(name, entry) == true then
          vim.lsp.enable(name)
          M._pending_enable[name] = nil
        end
      end
    end,
  })
end

-- Guarded enable. Only calls vim.lsp.enable when the server binary is
-- actually on PATH. Anything else is queued and retried when
-- mason-tool-installer fires MasonToolsUpdateCompleted.
--
-- opts.cmd (string): explicit binary path/name. Required for servers whose
--   lspconfig `cmd` is a function (jsonls, ts_ls, vtsls, yamlls, etc.) since
--   we can't introspect a function safely. The binary name is usually
--   visible in the lspconfig source for that server.
--
-- Without opts.cmd AND with a function `cmd`, we defer (never spawn) — this
-- is the safe default: missing spam beats spam.
function M.enable(name, opts)
  local avail = available(name, opts)
  if avail == true then
    vim.lsp.enable(name)
    M._pending_enable[name] = nil
    return
  end
  -- false or nil: queue and wait for mason to finish
  M._pending_enable[name] = opts or {}
  install_pending_hook()
end

function M.capabilities()
  local caps = vim.lsp.protocol.make_client_capabilities()
  caps = vim.tbl_deep_extend("force", caps, {
    textDocument = {
      completion = {
        completionItem = {
          documentationFormat = { "markdown", "plaintext" },
          snippetSupport = true,
          preselectSupport = true,
          insertReplaceSupport = true,
          labelDetailsSupport = true,
          deprecatedSupport = true,
          commitCharactersSupport = true,
          tagSupport = { valueSet = { 1 } },
          resolveSupport = {
            properties = { "documentation", "detail", "additionalTextEdits" },
          },
        },
      },
      foldingRange = { dynamicRegistration = false, lineFoldingOnly = true },
    },
  })
  -- blink.cmp contributes additional capabilities when loaded
  local ok, blink = pcall(require, "blink.cmp")
  if ok and type(blink.get_lsp_capabilities) == "function" then
    caps = blink.get_lsp_capabilities(caps)
  end
  return caps
end

function M.is_enabled(buf)
  if not buf or not vim.api.nvim_buf_is_valid(buf) then return false end
  return #vim.lsp.get_clients({ bufnr = buf }) > 0
end

function M.get_clients(buf)
  return vim.lsp.get_clients({ bufnr = buf or 0 })
end

function M.on_attach(fn)
  table.insert(attach_callbacks, fn)
end

function M._fire_attach(args)
  for _, fn in ipairs(attach_callbacks) do
    local ok, err = xpcall(function() fn(args) end, debug.traceback)
    if not ok then vim.notify("lib.lsp on_attach error: " .. err, vim.log.levels.ERROR) end
  end
end

function M.keymaps(bufnr)
  local map = function(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc, silent = true })
  end

  map("n", "gd", vim.lsp.buf.definition,       "LSP: definition")
  map("n", "gD", vim.lsp.buf.declaration,      "LSP: declaration")
  map("n", "gr", vim.lsp.buf.references,       "LSP: references")
  map("n", "gi", vim.lsp.buf.implementation,   "LSP: implementation")
  map("n", "gy", vim.lsp.buf.type_definition,  "LSP: type definition")
  map("n", "K",  vim.lsp.buf.hover,            "LSP: hover")
  map("n", "gK", vim.lsp.buf.signature_help,   "LSP: signature help")
  map("i", "<C-k>", vim.lsp.buf.signature_help,"LSP: signature help")
  map({ "n", "x" }, "<leader>ca", vim.lsp.buf.code_action, "LSP: code action")
  -- Note: <leader>cr is claimed by inc-rename.nvim (see editor/text.lua)
  -- for its incremental-preview rename, which is strictly better than the
  -- plain LSP rename. Defining it here would shadow it as a buffer-local.
  map("n", "<leader>cf", function() Lib.format.format({ buf = bufnr, force = true }) end, "Format buffer")
  map("n", "<leader>cl", "<cmd>checkhealth vim.lsp<cr>", "LSP: health")

  map("n", "<leader>uh", function()
    local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr })
    vim.lsp.inlay_hint.enable(not enabled, { bufnr = bufnr })
    vim.notify("Inlay hints: " .. (not enabled and "on" or "off"))
  end, "Toggle inlay hints")
end

function M.setup()
  -- diagnostic config
  local icons = Lib.icons.diagnostics
  vim.diagnostic.config({
    virtual_text = { spacing = 4, source = "if_many", prefix = "●" },
    signs = {
      text = {
        [vim.diagnostic.severity.ERROR] = icons.Error,
        [vim.diagnostic.severity.WARN]  = icons.Warn,
        [vim.diagnostic.severity.INFO]  = icons.Info,
        [vim.diagnostic.severity.HINT]  = icons.Hint,
      },
    },
    severity_sort = true,
    update_in_insert = false,
    float = { border = "rounded", source = "if_many" },
  })

  -- LspAttach wiring
  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("Lib.lsp", { clear = true }),
    callback = function(args)
      M.keymaps(args.buf)
      M._fire_attach(args)
    end,
  })
end

return M
