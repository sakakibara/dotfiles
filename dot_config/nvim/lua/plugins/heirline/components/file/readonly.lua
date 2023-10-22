local icons = require("config.icons")

return {
  {
    condition = function()
      return not vim.bo.modifiable or vim.bo.readonly
    end,
    provider = " " .. icons.status.Lock,
    hl = { fg = "orange" },
    update = "BufReadPost",
  },
}
