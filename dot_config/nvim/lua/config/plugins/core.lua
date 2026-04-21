-- lua/config/plugins/core.lua
return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    opts = {
      flavour = "mocha",
      background = { light = "latte", dark = "mocha" },
      integrations = {
        snacks = { enabled = true },
        which_key = true,
        native_lsp = { enabled = true },
        treesitter = true,
        heirline = true,  -- M3; no-op today, costs nothing
      },
    },
    config = function(_, opts)
      require("catppuccin").setup(opts)
      vim.cmd.colorscheme("catppuccin")
    end,
  },

  {
    "folke/snacks.nvim",
    name = "snacks.nvim",
    priority = 900,
    lazy = false,  -- keys spec below would flip us to lazy; keep snacks eager (Snacks global needed by many)
    opts = {
      bigfile   = { enabled = true },
      quickfile = { enabled = true },
      notifier  = { enabled = true },
      input     = { enabled = true },
      picker    = {
        enabled    = true,
        -- filename_first puts "lsp.lua   lua/lib/" instead of "lua/lib/lsp.lua",
        -- so the identifying token is always in the same column regardless of
        -- tree depth — much easier to skim than stock left-aligned paths.
        formatters = { file = { filename_first = true } },
      },
      terminal  = { enabled = true },
      bufdelete = { enabled = true },
      words     = { enabled = true },   -- LSP document_highlight under cursor
    },
    keys = {
      { "<leader>ff", function() Snacks.picker.files() end,                   desc = "Find files" },
      { "<leader>fg", function() Snacks.picker.grep() end,                    desc = "Live grep" },
      { "<leader>fb", function() Snacks.picker.buffers() end,                 desc = "Buffers" },
      { "<leader>fr", function() Snacks.picker.recent() end,                  desc = "Recent files" },
      { "<leader>fc", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, desc = "Config files" },
      { "<leader>fh", function() Snacks.picker.help() end,                    desc = "Help" },
      { "<leader>fm", function() Snacks.picker.marks() end,                   desc = "Marks" },
      { "<leader>fk", function() Snacks.picker.keymaps() end,                 desc = "Keymaps" },
      { "<leader>sw", function() Snacks.picker.grep_word() end,               desc = "Grep word",  mode = { "n", "x" } },
      { "<leader>sh", function() Snacks.picker.highlights() end,              desc = "Highlights" },
      { "<leader>sc", function() Snacks.picker.command_history() end,         desc = "Command history" },
      { "<leader>sn", function() Snacks.picker.notifications() end,           desc = "Notifications" },
      { "<leader>cd", function() Snacks.picker.diagnostics_buffer() end,      desc = "Diagnostics (buffer)" },
      { "<leader>cD", function() Snacks.picker.diagnostics() end,             desc = "Diagnostics (workspace)" },
      -- gd is the one pre-gr* classic we keep: nvim 0.11+ added `grr/gri/grt/gra/grn/gO`
      -- but not a definition verb in that family. Keeping `gd` here preserves
      -- universal muscle memory; `grr/gri/grt` (buffer-local overrides on
      -- LspAttach, see lib/lsp.lua) cover references / implementations / type.
      { "gd",         function() Snacks.picker.lsp_definitions() end,         desc = "LSP definitions" },
      { "<leader>ss", function() Snacks.picker.lsp_symbols() end,             desc = "LSP symbols (buffer)" },
      { "<leader>sS", function() Snacks.picker.lsp_workspace_symbols() end,   desc = "LSP symbols (workspace)" },
      -- git
      { "<leader>gg", function() Snacks.lazygit() end,                        desc = "Lazygit (root)" },
      { "<leader>gG", function() Snacks.lazygit({ cwd = vim.uv.cwd() }) end,  desc = "Lazygit (cwd)" },
      { "<leader>gl", function() Snacks.lazygit.log_file() end,               desc = "Lazygit log (file)" },
      { "<leader>gL", function() Snacks.lazygit.log() end,                    desc = "Lazygit log (all)" },
      { "<leader>gf", function() Snacks.picker.git_files() end,               desc = "Git files" },
      { "<leader>gs", function() Snacks.picker.git_status() end,              desc = "Git status" },
      { "<leader>gc", function() Snacks.picker.git_log() end,                 desc = "Git commits" },
      { "<leader>gb", function() Snacks.picker.git_branches() end,            desc = "Git branches" },
      -- terminal
      { "<C-/>",      function() Snacks.terminal() end,                        desc = "Terminal", mode = { "n", "t" } },
      { "<C-_>",      function() Snacks.terminal() end,                        desc = "Terminal (TTY alias)", mode = { "n", "t" } },
      -- buffers
      { "<leader>bd", function() Snacks.bufdelete() end,                       desc = "Delete buffer" },
      { "<leader>bD", function() Snacks.bufdelete.all() end,                   desc = "Delete all buffers" },
      { "<leader>bo", function() Snacks.bufdelete.other() end,                 desc = "Delete other buffers" },
      -- words (LSP reference navigation)
      { "]]",         function() Snacks.words.jump(1) end,                     desc = "Next reference" },
      { "[[",         function() Snacks.words.jump(-1) end,                    desc = "Prev reference" },
    },
  },

  {
    "folke/which-key.nvim",
    name = "which-key.nvim",
    event = "VeryLazy",
    opts = {
      preset = "helix",
      spec = {
        { "<leader>a", group = "ai (avante)" },
        { "<leader>b", group = "buffer" },
        { "<leader>c", group = "code (lsp)" },
        { "<leader>d", group = "debug (dap)" },
        { "<leader>f", group = "file / find" },
        { "<leader>g", group = "git" },
        { "<leader>gh", group = "git hunk" },
        { "<leader>q", group = "quit" },
        { "<leader>s", group = "search" },
        { "<leader>t", group = "test" },
        { "<leader>u", group = "ui / toggle" },
        { "<leader>x", group = "trouble / diagnostics" },
        { "<leader>z", group = "zk (notes)" },
      },
    },
  },
}
