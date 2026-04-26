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
      name = "plenary.nvim",
      lazy = true,
    },

    {
      "akinsho/flutter-tools.nvim",
      name = "flutter-tools.nvim",
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
        { "<Leader>cs", "<Cmd>FlutterRun<CR>", ft = "dart", desc = "Flutter Start" },
        { "<Leader>cE", "<Cmd>FlutterDevices<CR>", ft = "dart", desc = "Flutter Devices" },
        { "<Leader>ce", "<Cmd>FlutterEmulators<CR>", ft = "dart", desc = "Flutter Emulators" },
        { "<Leader>cl", "<Cmd>FlutterReload<CR>", ft = "dart", desc = "Flutter Reload" },
        { "<Leader>cR", "<Cmd>FlutterRestart<CR>", ft = "dart", desc = "Flutter Restart" },
        { "<Leader>cq", "<Cmd>FlutterQuit<CR>", ft = "dart", desc = "Flutter Quit" },
        { "<Leader>cD", "<Cmd>FlutterDetach<CR>", ft = "dart", desc = "Flutter Detach" },
        { "<Leader>cr", "<Cmd>FlutterRename<CR>", ft = "dart", desc = "Flutter Rename" },
      },
    },
  },
})
