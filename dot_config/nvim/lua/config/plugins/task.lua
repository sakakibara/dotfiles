-- Task runner and quickfix UI. Overseer streams long-running shell output
-- into a task panel; quicker.nvim replaces the default quickfix window
-- with an editable buffer.

return {
  -- Overseer: generic task runner. `:Grep pattern` runs grepprg and streams
  -- results into a quickfix list via the on_output_quickfix component.
  {
    "stevearc/overseer.nvim",
    cmd  = { "OverseerOpen", "OverseerClose", "OverseerToggle", "OverseerRun", "OverseerBuild", "Grep" },
    keys = {
      { "<Leader>uo", "<Cmd>OverseerToggle<CR>", desc = "Overseer toggle" },
      { "<Leader>uO", "<Cmd>OverseerRun<CR>",    desc = "Overseer run task" },
    },
    opts = {},
    config = function(_, opts)
      require("overseer").setup(opts)
      vim.api.nvim_create_user_command("Grep", function(params)
        local cmd, n = vim.o.grepprg:gsub("%$%*", params.args)
        if n == 0 then cmd = cmd .. " " .. params.args end
        local task = require("overseer").new_task({
          cmd = vim.fn.expandcmd(cmd),
          components = {
            { "on_output_quickfix", errorformat = vim.o.grepformat, open = not params.bang, open_height = 8, items_only = true },
            { "on_complete_dispose", timeout = 30 },
            "default",
          },
        })
        task:start()
      end, { nargs = "*", bang = true, complete = "file" })
    end,
  },

  -- Editable quickfix / loclist
  {
    "stevearc/quicker.nvim",
    event = "VeryLazy",
    keys = {
      { "<Leader>xq", function() require("quicker").toggle()                   end, desc = "Quickfix (quicker)" },
      { "<Leader>xl", function() require("quicker").toggle({ loclist = true }) end, desc = "Loclist (quicker)" },
    },
    opts = {},
  },
}
