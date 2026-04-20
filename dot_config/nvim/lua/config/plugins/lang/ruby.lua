-- lua/config/plugins/lang/ruby.lua
if vim.fn.executable("ruby") == 0 then return {} end

Lib.mason.add("ruby-lsp", "rubocop")
-- erb-formatter not in mason registry; install via `gem install erb-formatter`.

Lib.neotest.add("neotest-rspec", function() return require("neotest-rspec") end)

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "ruby" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("ruby_lsp", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("ruby_lsp")
  vim.lsp.config("rubocop", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("rubocop")
end)

Lib.plugin.on_load("conform.nvim", function()
  local conform = require("conform")
  conform.formatters_by_ft.ruby = { "rubocop" }
  conform.formatters_by_ft.eruby = { "erb_format" }
end)

return {
  {
    "suketa/nvim-dap-ruby",
    name = "nvim-dap-ruby",
    ft = "ruby",
    dependencies = { "nvim-dap" },
    config = function() require("dap-ruby").setup() end,
  },
  {
    "olimorris/neotest-rspec",
    name = "neotest-rspec",
    ft = "ruby",
    dependencies = { "neotest" },
  },
}
