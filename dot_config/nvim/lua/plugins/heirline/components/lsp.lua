local conditions = require("heirline.conditions")

return {
  condition = conditions.lsp_attached,
  update = { "LspAttach", "LspDetach" },
  provider = function()
    local names = {}
    for _, server in pairs(vim.lsp.get_clients({ bufnr = 0 })) do
      table.insert(names, server.name)
    end
    return " " .. Util.config.icons.status.Lsp .. table.concat(names, " ") .. " "
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
