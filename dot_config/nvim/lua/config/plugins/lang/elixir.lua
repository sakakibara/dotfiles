-- lua/config/plugins/lang/elixir.lua
if vim.fn.executable("elixir") == 0 then return {} end

Lib.mason.add("elixir-ls")
-- credo not in mason registry; user installs via `mix archive.install hex credo`.

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "elixir", "heex", "eex" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("elixirls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("elixirls")
end)

Lib.plugin.on_load("nvim-lint", function()
  if vim.fn.executable("credo") == 1 then
    require("lint").linters_by_ft.elixir = { "credo" }
  end
end)

Lib.neotest.add("neotest-elixir", function() return require("neotest-elixir") end)

return {
  {
    "jfpedroza/neotest-elixir",
    name = "neotest-elixir",
    ft = "elixir",
    dependencies = { "neotest" },
  },
}
