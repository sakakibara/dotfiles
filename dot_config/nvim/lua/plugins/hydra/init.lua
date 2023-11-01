return {
  {
    "anuvyklack/hydra.nvim",
    event = "VeryLazy",
    config = function()
      require("plugins.hydra.side_scroll")
      require("plugins.hydra.win")
    end,
  },

  {
    "sindrets/winshift.nvim",
    cmd = { "WinShift" },
    opts = {},
  },

  {
    "mrjones2014/smart-splits.nvim",
    lazy = true,
    opts = {},
  },

  {
    "anuvyklack/windows.nvim",
    dependencies = { "anuvyklack/middleclass" },
    cmd = { "WindowsMaximize" },
    opts = {},
  },
}
