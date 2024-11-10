require("config").init()

return {
  { "folke/lazy.nvim", version = "*" },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    opts = function()
      return {
        notifier = { enabled = not Util.plugin.has("noice.nvim") },
        statuscolumn = { enabled = true, folds = { open = true } },
      }
    end,
    keys = {
      {
        "<Leader>un",
        function()
          Snacks.notifier.hide()
        end,
        desc = "Dismiss all notifications",
      },
    },
  },
}
