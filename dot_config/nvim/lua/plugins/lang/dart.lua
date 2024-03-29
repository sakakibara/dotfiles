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
      { "<leader>cs", "<cmd>FlutterRun<cr>", ft = "dart", desc = "Flutter start" },
      { "<leader>cE", "<cmd>FlutterDevices<cr>", ft = "dart", desc = "Flutter devices" },
      { "<leader>ce", "<cmd>FlutterEmulators<cr>", ft = "dart", desc = "Flutter emulators" },
      { "<leader>cl", "<cmd>FlutterReload<cr>", ft = "dart", desc = "Flutter reload" },
      { "<leader>cR", "<cmd>FlutterRestart<cr>", ft = "dart", desc = "Flutter restart" },
      { "<leader>cq", "<cmd>FlutterQuit<cr>", ft = "dart", desc = "Flutter quit" },
      { "<leader>cD", "<cmd>FlutterDetach<cr>", ft = "dart", desc = "Flutter detach" },
      { "<leader>cr", "<cmd>FlutterRename<cr>", ft = "dart", desc = "Flutter rename" },
    },
  },
}
