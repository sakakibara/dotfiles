vim.treesitter.language.register("eex", { "eelixir" })

-- credo is not in the mason registry (`mix archive.install hex credo`).
return Lib.lang.setup({
  cmd = "elixir",
  ft = { "elixir", "heex", "eelixir", "surface" },
  mason = { "elixir-ls" },
  parsers = { "elixir", "heex", "eex", "surface" },
  servers = { elixirls = {} },
  linters = { elixir = { "credo" } },
  neotest = { ["neotest-elixir"] = function() return require("neotest-elixir") end },
  plugins = {
    {
      "jfpedroza/neotest-elixir",
      ft = "elixir",
      dependencies = { "neotest" },
    },
  },
})
