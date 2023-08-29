local M = {}

function M.diff_source()
  local icons = require("config.icons")
  ---@diagnostic disable-next-line: undefined-field
  local gitsigns = vim.b.gitsigns_status_dict
  if gitsigns then
    return {
      added = icons.git.added,
      modified = icons.git.modified,
      removed = icons.git.removed,
    }
  end
end

return M
