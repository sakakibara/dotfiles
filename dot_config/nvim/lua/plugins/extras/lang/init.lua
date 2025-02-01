local node_installed = vim.fn.executable("node") == 1
local python_installed = vim.fn.executable("python3") == 1

return {
  {
    import = "plugins.extras.lang.angular",
    enabled = function()
      return node_installed
    end,
  },
  {
    import = "plugins.extras.lang.ansible",
    enabled = function()
      return vim.fn.executable("ansible") == 1
    end,
  },
  {
    import = "plugins.extras.lang.astro",
    enabled = function()
      return node_installed
    end,
  },
  {
    import = "plugins.extras.lang.c",
    enabled = function()
      return vim.fn.executable("gcc") == 1
    end,
  },
  {
    import = "plugins.extras.lang.clojure",
    enabled = function()
      return vim.fn.executable("clj") == 1
    end,
  },
  {
    import = "plugins.extras.lang.csharp",
    enabled = function()
      return vim.fn.executable("dotnet") == 1
    end,
  },
  {
    import = "plugins.extras.lang.dart",
    enabled = function()
      return vim.fn.executable("dart") == 1
    end,
  },
  {
    import = "plugins.extras.lang.docker",
    enabled = function()
      return vim.fn.executable("docker") == 1
    end,
  },
  {
    import = "plugins.extras.lang.elixir",
    enabled = function()
      return vim.fn.executable("elixir") == 1
    end,
  },
  {
    import = "plugins.extras.lang.elm",
    enabled = function()
      return vim.fn.executable("elm") == 1
    end,
  },
  {
    import = "plugins.extras.lang.erlang",
    enabled = function()
      return vim.fn.executable("erl") == 1
    end,
  },
  {
    import = "plugins.extras.lang.git",
    enabled = function()
      return vim.fn.executable("git") == 1
    end,
  },
  {
    import = "plugins.extras.lang.gleam",
    enabled = function()
      return vim.fn.executable("gleam") == 1
    end,
  },
  {
    import = "plugins.extras.lang.go",
    enabled = function()
      return vim.fn.executable("go") == 1
    end,
  },
  {
    import = "plugins.extras.lang.haskell",
    enabled = function()
      return vim.fn.executable("ghc") == 1
    end,
  },
  {
    import = "plugins.extras.lang.helm",
    enabled = function()
      return vim.fn.executable("helm") == 1
    end,
  },
  {
    import = "plugins.extras.lang.java",
    enabled = function()
      return vim.fn.executable("java") == 1
    end,
  },
  {
    import = "plugins.extras.lang.json",
    enabled = true,
  },
  {
    import = "plugins.extras.lang.kotlin",
    enabled = function()
      return vim.fn.executable("kotlin") == 1
    end,
  },
  {
    import = "plugins.extras.lang.markdown",
    enabled = true,
  },
  {
    import = "plugins.extras.lang.nix",
    enabled = function()
      return vim.fn.executable("nix") == 1
    end,
  },
  {
    import = "plugins.extras.lang.nushell",
    enabled = function()
      return vim.fn.executable("nu") == 1
    end,
  },
  {
    import = "plugins.extras.lang.ocaml",
    enabled = function()
      return vim.fn.executable("ocaml") == 1
    end,
  },
  {
    import = "plugins.extras.lang.php",
    enabled = function()
      return vim.fn.executable("php") == 1
    end,
  },
  {
    import = "plugins.extras.lang.plantuml",
    enabled = true,
  },
  {
    import = "plugins.extras.lang.powershell",
    enabled = function()
      return vim.fn.executable("pwsh") == 1
    end,
  },
  {
    import = "plugins.extras.lang.prisma",
    enabled = node_installed,
  },
  {
    import = "plugins.extras.lang.python",
    enabled = python_installed,
  },
  {
    import = "plugins.extras.lang.ruby",
    enabled = function()
      return vim.fn.executable("ruby") == 1
    end,
  },
  {
    import = "plugins.extras.lang.rust",
    enabled = function()
      return vim.fn.executable("rustc") == 1
    end,
  },
  {
    import = "plugins.extras.lang.scala",
    enabled = function()
      return vim.fn.executable("scala") == 1
    end,
  },
  {
    import = "plugins.extras.lang.sql",
    enabled = true,
  },
  {
    import = "plugins.extras.lang.svelte",
    enabled = node_installed,
  },
  {
    import = "plugins.extras.lang.tailwind",
    enabled = node_installed,
  },
  {
    import = "plugins.extras.lang.terraform",
    enabled = function()
      return vim.fn.executable("terraform") == 1
    end,
  },
  {
    import = "plugins.extras.lang.tex",
    enabled = function()
      return vim.fn.executable("pdflatex") == 1
    end,
  },
  {
    import = "plugins.extras.lang.thrift",
    enabled = function()
      return vim.fn.executable("thrift") == 1
    end,
  },
  {
    import = "plugins.extras.lang.toml",
    enabled = true,
  },
  {
    import = "plugins.extras.lang.typescript",
    enabled = node_installed,
  },
  {
    import = "plugins.extras.lang.vue",
    enabled = node_installed,
  },
  {
    import = "plugins.extras.lang.yaml",
    enabled = true,
  },
  {
    import = "plugins.extras.lang.zig",
    enabled = function()
      return vim.fn.executable("zig") == 1
    end,
  },
}
