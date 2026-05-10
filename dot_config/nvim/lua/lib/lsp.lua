-- LSP integration: setup, capabilities, buffer keymaps, attach callbacks.
-- Called from config/init.lua at VeryLazy.

local M = {}

local attach_callbacks = {}            -- { fn, fn, ... } — fired for every attach
local attach_callbacks_by_name = {}    -- { [client_name] = { fn, ... } } — name-filtered

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

-- Memoized result. The cache is invalidated when blink's load state flips,
-- so the first batch of server configs (registered before blink loads, when
-- lspconfig fires on LazyFile) gets caps without blink, and any later call —
-- after blink finishes loading on InsertEnter — recomputes once with blink.
local _caps_cache = nil
local _caps_has_blink = false

function M.capabilities()
  local ok, blink = pcall(require, "blink.cmp")
  local has_blink = ok and type(blink.get_lsp_capabilities) == "function"
  if _caps_cache and _caps_has_blink == has_blink then
    return _caps_cache
  end
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
  if has_blink then
    caps = blink.get_lsp_capabilities(caps)
  end
  _caps_cache = caps
  _caps_has_blink = has_blink
  return caps
end

function M.is_enabled(buf)
  if not buf or not vim.api.nvim_buf_is_valid(buf) then return false end
  return #vim.lsp.get_clients({ bufnr = buf }) > 0
end

function M.get_clients(buf)
  return vim.lsp.get_clients({ bufnr = buf or 0 })
end

-- Two forms:
--   M.on_attach(fn)           — fires on every LspAttach; fn(args)
--   M.on_attach(name, fn)     — fires only when client.name == name; fn(args, client)
-- The name-keyed form is the preferred path: dispatcher does an O(1) lookup
-- so N server-specific hooks don't each pay a name-filter cost on every
-- attach. Use the unconditional form only when the hook genuinely applies
-- to all servers.
function M.on_attach(name_or_fn, fn)
  if type(name_or_fn) == "function" then
    table.insert(attach_callbacks, name_or_fn)
  else
    local list = attach_callbacks_by_name[name_or_fn]
    if not list then
      list = {}
      attach_callbacks_by_name[name_or_fn] = list
    end
    table.insert(list, fn)
  end
end

function M._fire_attach(args)
  for _, fn in ipairs(attach_callbacks) do
    local ok, err = xpcall(function() fn(args) end, debug.traceback)
    if not ok then vim.notify("lib.lsp on_attach error: " .. err, vim.log.levels.ERROR) end
  end
  local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
  if not client then return end
  local by_name = attach_callbacks_by_name[client.name]
  if not by_name then return end
  for _, fn in ipairs(by_name) do
    local ok, err = xpcall(function() fn(args, client) end, debug.traceback)
    if not ok then vim.notify("lib.lsp on_attach error: " .. err, vim.log.levels.ERROR) end
  end
end

function M.keymaps(bufnr)
  local function bmap(mode, lhs, rhs, opts)
    opts = opts or {}
    opts.buffer = bufnr
    opts.silent = opts.silent ~= false
    vim.keymap.set(mode, lhs, rhs, opts)
  end

  -- LSP navigation follows Neovim 0.11+'s `gr*` namespace so muscle memory
  -- transfers to any stock Neovim. Where a Snacks picker gives a richer
  -- experience than nvim's default, we override the default here. Stock
  -- defaults kept: `gra` (code action), `gO` (document symbols), `K` (hover).
  -- `<C-s>` (insert: signature help) is shadowed by our save binding; use
  -- `<C-k>` below instead. `gd` → Snacks picker is set globally in core.lua
  -- to fill the gap nvim left (there's no `grd` default, and `gd` is
  -- universal muscle memory).
  bmap("n", "gD", vim.lsp.buf.declaration,                           { desc = "LSP: declaration" })
  bmap("n", "grr", function() Snacks.picker.lsp_references()       end, { desc = "LSP: references" })
  bmap("n", "gri", function() Snacks.picker.lsp_implementations()  end, { desc = "LSP: implementations" })
  bmap("n", "grt", function() Snacks.picker.lsp_type_definitions() end, { desc = "LSP: type definition" })

  -- grn: inc-rename's preview UI overrides nvim's plain rename default.
  -- Our LspAttach fires after nvim's defaults, so this buffer-local
  -- override wins. :IncRename lazy-loads inc-rename.nvim via its cmd trigger.
  bmap("n", "grn",
    function() return ":IncRename " .. vim.fn.expand("<cword>") end,
    { desc = "LSP: rename (incremental preview)", expr = true })

  bmap("i", "<C-k>", vim.lsp.buf.signature_help, { desc = "LSP: signature help" })

  bmap("n", "<Leader>cf",
    function() Lib.format.format({ buf = bufnr, force = true }) end,
    { desc = "Format buffer" })
  bmap("n", "<Leader>cl", "<Cmd>checkhealth vim.lsp<CR>", { desc = "LSP: health" })

  bmap("n", "<Leader>uh", function()
    local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr })
    vim.lsp.inlay_hint.enable(not enabled, { bufnr = bufnr })
    local now_on = not enabled
    vim.notify(
      ("**%s**"):format(now_on and "on" or "off"),
      now_on and vim.log.levels.INFO or vim.log.levels.WARN,
      { title = "Inlay hints " .. (now_on and "on" or "off") }
    )
  end, { desc = "Toggle inlay hints" })
end

function M.setup()
  -- diagnostic config
  local icons = Lib.icons.diagnostics
  local sev = vim.diagnostic.severity
  local sev_name = { [sev.ERROR] = "Error", [sev.WARN] = "Warn", [sev.INFO] = "Info", [sev.HINT] = "Hint" }
  vim.diagnostic.config({
    -- Per-severity prefix icon: the inline message inherits its virtual-text
    -- highlight automatically, so no explicit hl return is needed.
    virtual_text = {
      spacing = 4,
      source  = "if_many",
      prefix  = function(d) return icons[sev_name[d.severity]] .. " " end,
    },
    signs = {
      text = {
        [sev.ERROR] = icons.Error,
        [sev.WARN]  = icons.Warn,
        [sev.INFO]  = icons.Info,
        [sev.HINT]  = icons.Hint,
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
