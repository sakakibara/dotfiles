local LazyUtil = require("lazy.core.util")

local M = {}

local KeymapsUtil = {}

function KeymapsUtil.put_empty_line(put_above)
  if type(put_above) == 'boolean' then
    vim.o.operatorfunc = 'v:lua.KeymapsUtil.put_empty_line'
    KeymapsUtil.cache_empty_line = { put_above = put_above }
    return 'g@l'
  end

  local target_line = vim.fn.line('.') - (KeymapsUtil.cache_empty_line.put_above and 1 or 0)
  ---@diagnostic disable-next-line: param-type-mismatch
  vim.fn.append(target_line, vim.fn['repeat']({ '' }, vim.v.count1))
end

function M.setup()
  _G.KeymapsUtil = KeymapsUtil
end

function M.toggle(option, silent, values)
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
function M.toggle_number()
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
function M.toggle_diagnostics()
  diagnostic_enabled = not diagnostic_enabled
  if diagnostic_enabled then
    vim.diagnostic.enable()
    LazyUtil.info("Enabled diagnostics", { title = "Diagnostics" })
  else
    vim.diagnostic.disable()
    LazyUtil.warn("Disabled diagnostics", { title = "Diagnostics" })
  end
end

return M
