local conditions = require("heirline.conditions")
local icons = require("config.icons")

return {
  condition = conditions.lsp_attached,
  update = { "LspAttach", "LspDetach", "BufEnter" },
  provider = icons.status.Lsp .. "LSP" .. " ",
  hl = { fg = "green", bold = true },
  on_click = {
    name = "heirline_LSP",
    callback = function()
      vim.schedule(function()
        vim.cmd("LspInfo")
      end)
    end,
  },
}
