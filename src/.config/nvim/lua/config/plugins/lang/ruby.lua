return Lib.lang.setup({
  cmd = "ruby",
  mason = { "ruby-lsp", "rubocop", "erb-formatter" },
  parsers = { "ruby" },
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
