-- lua/config/plugins/lang/dart.lua
-- Dart LSP is bundled with the Dart SDK (via `dart language-server`); no mason
-- entry. flutter-tools.nvim owns the LSP lifecycle, so we don't register dartls
-- through nvim-lspconfig either.
return Lib.lang.setup({
  cmd = "dart",
  parsers = { "dart" },
  formatters = { dart = { "dart_format" } },
  plugins = {
    {
      "nvim-lua/plenary.nvim",
      lazy = true,
    },

    {
      "akinsho/flutter-tools.nvim",
      ft = "dart",
      cmd = {
        "FlutterRun",
        "FlutterDevices",
        "FlutterEmulators",
        "FlutterReload",
        "FlutterRestart",
        "FlutterQuit",
        "FlutterDetach",
        "FlutterOutlineToggle",
        "FlutterDevTools",
        "FlutterDevToolsActivate",
        "FlutterCopyProfilerUrl",
        "FlutterLspRestart",
        "FlutterSuper",
        "FlutterReanalyze",
        "FlutterRename",
      },
      dependencies = { "plenary.nvim" },
      opts = {
        debugger = {
          enabled = true,
          run_via_dap = true,
          exception_breakpoints = "default",
        },
        dev_log = {
          enabled = false,
        },
      },
      keys = {
        { "<Leader>cs", "<Cmd>FlutterRun<CR>", ft = "dart", desc = "Flutter start" },
        { "<Leader>cE", "<Cmd>FlutterDevices<CR>", ft = "dart", desc = "Flutter devices" },
        { "<Leader>ce", "<Cmd>FlutterEmulators<CR>", ft = "dart", desc = "Flutter emulators" },
        { "<Leader>cl", "<Cmd>FlutterReload<CR>", ft = "dart", desc = "Flutter reload" },
        { "<Leader>cR", "<Cmd>FlutterRestart<CR>", ft = "dart", desc = "Flutter restart" },
        { "<Leader>cq", "<Cmd>FlutterQuit<CR>", ft = "dart", desc = "Flutter quit" },
        { "<Leader>cD", "<Cmd>FlutterDetach<CR>", ft = "dart", desc = "Flutter detach" },
        { "<Leader>cr", "<Cmd>FlutterRename<CR>", ft = "dart", desc = "Flutter rename" },
      },
    },
  },
})
