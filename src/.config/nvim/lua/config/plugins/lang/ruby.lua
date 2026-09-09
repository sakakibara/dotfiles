vim.treesitter.language.register("embedded_template", { "eruby" })

return Lib.lang.setup({
  cmd = "ruby",
  ft = { "ruby", "eruby" },
  mason = { "ruby-lsp", "rubocop", "erb-formatter" },
  parsers = { "ruby", "embedded_template" },
  servers = {
    -- ruby_lsp's lspconfig `cmd` is a function, so Lib.lsp.enable's
    -- availability check needs an explicit binary hint (otherwise it
    -- queues forever and never enables).
    ruby_lsp = { binary = "ruby-lsp" },
    rubocop = {},
  },
  formatters = {
    ruby = { "rubocop" },
    eruby = { "erb_format" },
  },
  neotest = { ["neotest-rspec"] = function() return require("neotest-rspec") end },
  plugins = {
    {
      "suketa/nvim-dap-ruby",
      ft = "ruby",
      dependencies = { "nvim-dap" },
      config = function() require("dap-ruby").setup() end,
    },
    {
      "olimorris/neotest-rspec",
      ft = "ruby",
      dependencies = { "neotest" },
    },
  },
})
