require("config").init()

return {
  { "folke/lazy.nvim", version = "*" },
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    keys = {
      {
        "<Leader>un",
        function()
          Snacks.notifier.hide()
        end,
        desc = "Dismiss all notifications",
      },
    },
    opts = function()
      return {
        notifier = { enabled = true },
        quickfile = { enabled = true },
        bigfile = { enabled = true },
        words = { enabled = true },
        statuscolumn = { enabled = true, folds = { open = true } },
      }
    end,
    config = function(_, opts)
      local notify = vim.notify
      require("snacks").setup(opts)
      if Util.plugin.has("noice.nvim") then
        vim.notify = notify
      end
    end,
  },
}
