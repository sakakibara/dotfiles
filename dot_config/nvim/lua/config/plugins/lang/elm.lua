-- lua/config/plugins/lang/elm.lua
if vim.fn.executable("elm") == 0 then return {} end

-- elm-language-server and elm-format not in the mason reference table; omit.
-- Install elm-language-server via `npm i -g @elm-tooling/elm-language-server`
-- and elm-format via `npm i -g elm-format` if needed.

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "elm" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("elmls", { capabilities = Lib.lsp.capabilities() })
  Lib.lsp.enable("elmls")
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.elm = { "elm_format" }
end)

return {}
