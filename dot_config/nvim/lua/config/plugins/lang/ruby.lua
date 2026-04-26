-- lua/config/plugins/lang/ruby.lua
-- erb-formatter not in mason registry; install via `gem install erb-formatter`.
return Lib.lang.setup({
  cmd = "ruby",
  mason = { "ruby-lsp", "rubocop" },
  parsers = { "ruby" },
  servers = {
    ruby_lsp = {},
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
  },
})
