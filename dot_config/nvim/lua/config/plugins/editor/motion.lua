-- lua/config/plugins/editor/motion.lua
-- Movement: treesitter-aware jumps, quick-jump marks.

return {
  -- Flash: s/S treesitter jump, remote-op (r/R), cmdline toggle
  {
    "folke/flash.nvim",
    name = "flash.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      { "s",     function() require("flash").jump()              end, mode = { "n", "x", "o" }, desc = "Flash" },
      { "S",     function() require("flash").treesitter()        end, mode = { "n", "x", "o" }, desc = "Flash treesitter" },
      { "r",     function() require("flash").remote()            end, mode = "o",               desc = "Remote flash" },
      { "R",     function() require("flash").treesitter_search() end, mode = { "o", "x" },      desc = "Treesitter search" },
      { "<C-s>", function() require("flash").toggle()            end, mode = "c",               desc = "Toggle flash search" },
    },
  },

  -- Harpoon: pin up to 5 files per project, jump with <Leader>1..5
  {
    "ThePrimeagen/harpoon",
    name = "harpoon",
    branch = "harpoon2",
    dependencies = { "plenary.nvim" },
    keys = {
      { "<Leader>H", function() require("harpoon"):list():add() end,                                 desc = "Harpoon add" },
      { "<Leader>h", function() local h = require("harpoon"); h.ui:toggle_quick_menu(h:list()) end, desc = "Harpoon menu" },
      { "<Leader>1", function() require("harpoon"):list():select(1) end, desc = "Harpoon 1" },
      { "<Leader>2", function() require("harpoon"):list():select(2) end, desc = "Harpoon 2" },
      { "<Leader>3", function() require("harpoon"):list():select(3) end, desc = "Harpoon 3" },
      { "<Leader>4", function() require("harpoon"):list():select(4) end, desc = "Harpoon 4" },
      { "<Leader>5", function() require("harpoon"):list():select(5) end, desc = "Harpoon 5" },
    },
    opts = {},
  },

  -- plenary is shared by harpoon + text-case + lsp-file-operations
  { "nvim-lua/plenary.nvim", name = "plenary.nvim", lazy = true },
}
