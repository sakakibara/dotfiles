-- lua/config/plugins/lang/dart.lua
if vim.fn.executable("dart") == 0 then return {} end

-- Dart LSP is bundled with the Dart SDK (via `dart language-server`); no mason entry.

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "dart" })
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.dart = { "dart_format" }
end)

return {
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
}
