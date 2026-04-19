-- lua/lib/format.lua
-- Format integration: BufWritePre hook, manual format, LSP fallback.

local M = {}

M._enabled = { global = true, buffers = {} }

function M.enabled(opts)
  opts = opts or {}
  local buf = opts.buf or vim.api.nvim_get_current_buf()
  if M._enabled.buffers[buf] == false then return false end
  if not M._enabled.global then return false end
  return true
end

function M.toggle(scope, buf)
  if scope == "global" then
    M._enabled.global = not M._enabled.global
    vim.notify("Format on save: " .. (M._enabled.global and "on" or "off"))
  elseif scope == "buffer" then
    buf = buf or vim.api.nvim_get_current_buf()
    local cur = M._enabled.buffers[buf]
    if cur == nil then cur = true end
    M._enabled.buffers[buf] = not cur
    vim.notify(("Format on save (buffer %d): %s"):format(buf, M._enabled.buffers[buf] and "on" or "off"))
  end
end

function M.format(opts)
  opts = opts or {}
  local buf = opts.buf or vim.api.nvim_get_current_buf()
  local async = opts.async == true  -- default sync

  local has_conform, conform = pcall(require, "conform")
  if has_conform then
    local formatters = conform.list_formatters_to_run(buf)
    if formatters and #formatters > 0 then
      conform.format({ bufnr = buf, timeout_ms = 1000, async = async })
      return
    end
  end

  -- LSP fallback: format via attached client that supports textDocument/formatting
  if #vim.lsp.get_clients({ bufnr = buf, method = "textDocument/formatting" }) > 0 then
    vim.lsp.buf.format({ bufnr = buf, async = async, timeout_ms = 1000 })
    return
  end

  vim.notify("No formatters available for this buffer", vim.log.levels.WARN)
end

-- Called from vim.opt.formatexpr via options.lua:
--   opt.formatexpr = "v:lua.Lib.format.formatexpr()"
function M.formatexpr()
  local has_conform, conform = pcall(require, "conform")
  if has_conform then
    return conform.formatexpr()
  end
  return 1  -- fall back to default LSP formatexpr
end

function M.setup()
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = vim.api.nvim_create_augroup("Lib.format", { clear = true }),
    callback = function(args)
      if M.enabled({ buf = args.buf }) then
        M.format({ buf = args.buf, async = false })
      end
    end,
  })
end

return M
