---@class util.terminal
local M = {}

function M.lazygit_toggle()
  local Terminal = require("toggleterm.terminal").Terminal

  local lazygit = Terminal:new({
    cmd = "lazygit",
    direction = "float",
    hidden = true,
    float_opts = {
      border = "none",
      width = 100000,
      height = 100000,
    },
    on_open = function(_)
      vim.cmd("startinsert!")
    end,
    on_close = function(_) end,
    count = 99,
  })

  lazygit:toggle()
end

return M
