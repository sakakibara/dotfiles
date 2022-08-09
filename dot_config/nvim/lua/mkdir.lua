local fn = vim.fn

local M = {}

function M.run(dir, force)
  local pattern = vim.regex('^y\\%[es]$')
  local message = "'" .. dir .. "' does not exist. Create? [y/N] "

  -- This handles URLs using netrw. See ':help netrw-transparent' for details.
  if dir:find('%l+://') == 1 then
    return
  end

  if fn.isdirectory(dir) == 0 then
    if force == 1 or pattern:match_str(fn.input(message)) then
      fn.mkdir(dir, 'p')
    end
  end
end

return M
