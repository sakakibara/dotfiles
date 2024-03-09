local LazyUtil = require("lazy.core.util")

local M = {}

function M.option(option, silent, values)
  if values then
    if vim.opt_local[option]:get() == values[1] then
      vim.opt_local[option] = values[2]
    else
      vim.opt_local[option] = values[1]
    end
    return LazyUtil.info("Set " .. option .. " to " .. vim.opt_local[option]:get(), { title = "Option" })
  end
  vim.opt_local[option] = not vim.opt_local[option]:get()
  if not silent then
    if vim.opt_local[option]:get() then
      LazyUtil.info("Enabled " .. option, { title = "Options" })
    else
      LazyUtil.warn("Disabled " .. option, { title = "Options" })
    end
  end
end

local nu = { number = true, relativenumber = true }
function M.number()
  if vim.opt_local.number:get() or vim.opt_local.relativenumber:get() then
    nu = { number = vim.opt_local.number:get(), relativenumber = vim.opt_local.relativenumber:get() }
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    LazyUtil.warn("Disabled line numbers", { title = "Options" })
  else
    vim.opt_local.number = nu.number
    vim.opt_local.relativenumber = nu.relativenumber
    LazyUtil.info("Enabled line numbers", { title = "Options" })
  end
end

local diagnostic_enabled = true
function M.diagnostics()
  if vim.diagnostic.is_disabled then
    diagnostic_enabled = not vim.diagnostic.is_disabled()
  end
  diagnostic_enabled = not diagnostic_enabled

  if diagnostic_enabled then
    vim.diagnostic.enable()
    LazyUtil.info("Enabled diagnostics", { title = "Diagnostics" })
  else
    vim.diagnostic.disable()
    LazyUtil.warn("Disabled diagnostics", { title = "Diagnostics" })
  end
end

function M.inlay_hints(buf, value)
  local inlay_hint = vim.lsp.buf.inlay_hint or vim.lsp.inlay_hint
  if type(inlay_hint) == "function" then
    inlay_hint(buf, value)
  elseif type(inlay_hint) == "table" and inlay_hint.enable then
    if value == nil then
      value = not inlay_hint.is_enabled(buf)
    end
    inlay_hint.enable(buf, value)
  end
end

setmetatable(M, {
  __call = function(m, ...)
    return m.option(...)
  end,
})

return M
