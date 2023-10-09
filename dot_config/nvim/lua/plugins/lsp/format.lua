local LazyUtil = require("lazy.core.util")

local M = {}

M.opts = nil

M.custom_format = nil

function M.enabled()
  return M.opts.autoformat
end

function M.toggle()
  if vim.b.autoformat == false then
    vim.b.autoformat = nil
    M.opts.autoformat = true
  else
    M.opts.autoformat = not M.opts.autoformat
  end
  if M.opts.autoformat then
    LazyUtil.info("Enabled format on save", { title = "Format" })
  else
    LazyUtil.warn("Disabled format on save", { title = "Format" })
  end
end

function M.format(opts)
  local buf = vim.api.nvim_get_current_buf()
  if vim.b.autoformat == false and not (opts and opts.force) then
    return
  end

  if
    M.custom_format
    and LazyUtil.try(function()
      return M.custom_format(buf)
    end, { msg = "Custom formatter failed" })
  then
    return
  end

  local formatters = M.get_formatters(buf)
  local client_ids = vim.tbl_map(function(client)
    return client.id
  end, formatters.active)

  if #client_ids == 0 then
    if opts and opts.force then
      Util.warn("No formatter available", { title = "Format" })
    end
    return
  end

  vim.lsp.buf.format(vim.tbl_deep_extend("force", {
    bufnr = buf,
    filter = function(client)
      return vim.tbl_contains(client_ids, client.id)
    end,
  }, require("util.lazy").opts("nvim-lspconfig").format or {}))
end

function M.get_formatters(bufnr)
  local ret = {
    active = {},
    available = {},
  }

  local clients = require("util.lsp").get_clients({ bufnr = bufnr })
  for _, client in ipairs(clients) do
    if M.supports_format(client) then
      table.insert(ret.available, client)
    end
  end

  return ret
end

function M.supports_format(client)
  if
    client.config
    and client.config.capabilities
    and client.config.capabilities.documentFormattingProvider == false
  then
    return false
  end
  return client.supports_method("textDocument/formatting") or client.supports_method("textDocument/rangeFormatting")
end

function M.setup(opts)
  M.opts = opts
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = vim.api.nvim_create_augroup("AutoFormat", {}),
    callback = function()
      if M.opts.autoformat then
        M.format()
      end
    end,
  })
end

return M
