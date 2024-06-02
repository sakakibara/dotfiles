---@class util.cmp
local M = {}

function M.confirm(opts)
  local cmp = require("cmp")
  opts = vim.tbl_extend("force", {
    select = true,
    behavior = cmp.ConfirmBehavior.Insert,
  }, opts or {})
  return function(fallback)
    if cmp.core.view:visible() or vim.fn.pumvisible() == 1 then
      Util.keymaps.create_undo()
      if cmp.confirm(opts) then
        return
      end
    end
    return fallback()
  end
end

return M
