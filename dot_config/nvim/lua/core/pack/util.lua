local M = {}

-- Combined vim.api.nvim_echo + vim.notify so the message persists in :messages
-- (the snacks notifier alone doesn't write there). Highlight derived from
-- level so warnings/errors stand out in the cmdline strip on snap.
function M.notify(msg, level)
  level = level or vim.log.levels.INFO
  local hl = (level >= vim.log.levels.ERROR and "ErrorMsg")
    or (level >= vim.log.levels.WARN and "WarningMsg")
    or "Normal"
  pcall(vim.api.nvim_echo, { { tostring(msg), hl } }, true, {})
  vim.notify(msg, level)
end

return M
