-- credo not in mason registry; user installs via `mix archive.install hex credo`.
local linters = {}
if vim.fn.executable("credo") == 1 then linters.elixir = { "credo" } end

return Lib.lang.setup({
  cmd = "elixir",
  ft = { "elixir", "heex", "eelixir", "surface" },
  mason = { "elixir-ls" },
  parsers = { "elixir", "heex", "eex", "surface" },
  servers = { elixirls = {} },
  linters = linters,
  neotest = { ["neotest-elixir"] = function() return require("neotest-elixir") end },
  plugins = {
    {
      "jfpedroza/neotest-elixir",
      ft = "elixir",
      dependencies = { "neotest" },
    },
  },
})
