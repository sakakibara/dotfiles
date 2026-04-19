-- lua/config/plugins/git.lua
-- Git integration: gitsigns (hunk signs + per-hunk actions), lazygit via
-- snacks for repository operations, blame and diff helpers.

return {
  {
    "lewis6991/gitsigns.nvim",
    name = "gitsigns.nvim",
    event = "LazyFile",
    opts = {
      signs = {
        add          = { text = "┃" },
        change       = { text = "┃" },
        delete       = { text = "_" },
        topdelete    = { text = "‾" },
        changedelete = { text = "┃" },
        untracked    = { text = "┃" },
      },
      signs_staged = {
        add          = { text = "▎" },
        change       = { text = "▎" },
        delete       = { text = "_" },
        topdelete    = { text = "‾" },
        changedelete = { text = "~" },
      },
      on_attach = function(buf)
        local gs = require("gitsigns")
        local map = function(mode, lhs, rhs, desc)
          vim.keymap.set(mode, lhs, rhs, { buffer = buf, desc = desc, silent = true })
        end

        map("n", "]h", function() gs.nav_hunk("next") end, "Next hunk")
        map("n", "[h", function() gs.nav_hunk("prev") end, "Prev hunk")
        map("n", "]H", function() gs.nav_hunk("last") end, "Last hunk")
        map("n", "[H", function() gs.nav_hunk("first") end, "First hunk")

        map({ "n", "v" }, "<leader>ghs", "<cmd>Gitsigns stage_hunk<cr>",    "Stage hunk")
        map({ "n", "v" }, "<leader>ghr", "<cmd>Gitsigns reset_hunk<cr>",    "Reset hunk")
        map("n", "<leader>ghS", gs.stage_buffer,                             "Stage buffer")
        map("n", "<leader>ghR", gs.reset_buffer,                             "Reset buffer")
        map("n", "<leader>ghu", gs.undo_stage_hunk,                          "Undo stage hunk")
        map("n", "<leader>ghp", gs.preview_hunk_inline,                      "Preview hunk")
        map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end, "Blame line")
        map("n", "<leader>ghd", gs.diffthis,                                 "Diff this")
        map("n", "<leader>ghD", function() gs.diffthis("~") end,             "Diff against HEAD~")

        map({ "o", "x" }, "ih", "<cmd>Gitsigns select_hunk<cr>", "Inner hunk")
      end,
    },
  },
}
