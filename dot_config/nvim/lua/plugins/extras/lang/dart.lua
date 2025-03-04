return {
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        dart = { "dart_format" },
      },
    },
  },

  {
    "akinsho/flutter-tools.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "stevearc/dressing.nvim",
    },
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
