-- lua/lib/lsp.lua
-- LSP integration: setup, capabilities, buffer keymaps, attach callbacks.
-- Called from config/init.lua at VeryLazy.

local M = {}

local attach_callbacks = {}  -- { fn, fn, ... }

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
  map("n", "<leader>cf", function() Lib.format.format({ buf = bufnr }) end, "Format buffer")
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
