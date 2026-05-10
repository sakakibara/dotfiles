local M = {}

function M.fg(name)
  local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
  if hl.fg then return string.format("#%06x", hl.fg) end
end

function M.bg(name)
  local hl = vim.api.nvim_get_hl(0, { name = name, link = false })
  if hl.bg then return string.format("#%06x", hl.bg) end
end

function M.hl(name, opts)
  vim.api.nvim_set_hl(0, name, opts)
end

return M
