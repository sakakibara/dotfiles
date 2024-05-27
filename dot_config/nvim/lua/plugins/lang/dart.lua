return {
  {
    "akinsho/flutter-tools.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "stevearc/dressing.nvim",
    },
    event = "LazyFile",
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
}
