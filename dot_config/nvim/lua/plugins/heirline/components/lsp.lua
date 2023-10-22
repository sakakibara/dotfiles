local conditions = require("heirline.conditions")
local icons = require("config.icons")

return {
  condition = conditions.lsp_attached,
  update = { "LspAttach", "LspDetach" },
  provider = function()
    local names = {}
    for _, server in pairs(vim.lsp.get_active_clients({ bufnr = 0 })) do
      table.insert(names, server.name)
    end
    return " " .. icons.status.Lsp .. table.concat(names, " ") .. " "
  end,
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
