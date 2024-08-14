return {
  {
    "nvim-telekasten/telekasten.nvim",
    dependencies = {
      "nvim-telescope/telescope.nvim",
      "nvim-telekasten/calendar-vim",
    },
    keys = {
      { "<Leader>z", "<Cmd>Telekasten panel<CR>", desc = "Telekasten" },
      { "<Leader>zf", "<Cmd>Telekasten find_notes<CR>", desc = "Find notes" },
      { "<Leader>zg", "<Cmd>Telekasten search_notes<CR>", desc = "Search notes" },
      { "<Leader>zd", "<Cmd>Telekasten goto_today<CR>", desc = "Goto today" },
      { "<Leader>zz", "<Cmd>Telekasten follow_link<CR>", desc = "Follow link" },
      { "<Leader>zn", "<Cmd>Telekasten new_note<CR>", desc = "New note" },
      { "<Leader>zc", "<Cmd>Telekasten show_calendar<CR>", desc = "Calendar" },
      { "<Leader>zb", "<Cmd>Telekasten show_backlinks<CR>", desc = "Backlinks" },
      { "<Leader>zi", "<Cmd>Telekasten insert_link<CR>", desc = "Insert link" },
      { "<Leader>zI", "<Cmd>Telekasten insert_img_link<CR>", desc = "Insert image" },
    },
    opts = {
      home = vim.fn.expand("~/Notes"),
      dailies = vim.fn.expand("~/Notes/daily"),
      weeklies = vim.fn.expand("~/Notes/weekly"),
      auto_set_filetype = false,
      tag_notation = "@tag",
    },
  },
}
