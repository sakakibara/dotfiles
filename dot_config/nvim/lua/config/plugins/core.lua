-- lua/config/plugins/core.lua
local theme = Lib.theme.read()
local catppuccin_active = theme.family == "catppuccin"
local catppuccin_variant = catppuccin_active and theme.variant or "mocha"

return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    opts = {
      flavour = catppuccin_variant,
      background = { light = "latte", dark = "mocha" },
      integrations = {
        snacks = { enabled = true },
        which_key = true,
        native_lsp = { enabled = true },
        treesitter = true,
        treesitter_context = true,  -- TreesitterContext / Bottom / LineNumber hl groups
        heirline = true,  -- M3; no-op today, costs nothing
      },
    },
    config = function(_, opts)
      require("catppuccin").setup(opts)
      if catppuccin_active then
        vim.cmd.colorscheme("catppuccin-" .. opts.flavour)
      end
    end,
  },

  {
    "folke/snacks.nvim",
    name = "snacks.nvim",
    priority = 900,
    lazy = false,  -- keys spec below would flip us to lazy; keep snacks eager (Snacks global needed by many)
    config = function(_, opts)
      require("snacks").setup(opts)
      -- Diagnostic: wrap snacks notifier to log every notification call.
      -- vim.notify gets replaced by snacks itself, so wrapping at the
      -- vim.notify level (in config/init.lua) doesn't catch post-snacks
      -- calls. Wrap snacks's own notify entrypoint instead.
      pcall(function()
        local snacks_notif = require("snacks.notifier")
        local orig = snacks_notif.notify
        local logf = io.open("/tmp/pack-flash-trace.log", "a")
        if logf then
          snacks_notif.notify = function(msg, level, nopts)
            logf:write(("[%s] snacks.notify: %s\n"):format(
              os.date("%H:%M:%S"),
              tostring(msg):sub(1, 120)))
            logf:flush()
            return orig(msg, level, nopts)
          end
        end
      end)
    end,
    opts = {
      bigfile   = {
        enabled = true,
        -- Override the default setup. Stock snacks re-enables `syntax = ft`
        -- on a scheduled tick, which re-arms Vim's regex syntax engine — and
        -- that's what freezes nvim on multi-MB single lines (minified bundles,
        -- panic logs). We keep the rest of the disables and add wrap/cursorline.
        setup = function(ctx)
          if vim.fn.exists(":NoMatchParen") ~= 0 then
            vim.cmd("NoMatchParen")
          end
          Snacks.util.wo(0, {
            foldmethod    = "manual",
            statuscolumn  = "",
            conceallevel  = 0,
            wrap          = false,
            cursorline    = false,
          })
          vim.bo[ctx.buf].synmaxcol = 200
          vim.b.minianimate_disable = true
        end,
      },
      quickfile = { enabled = true },
      notifier  = { enabled = true },
      input     = { enabled = true },
      picker    = {
        enabled    = true,
        -- filename_first puts "lsp.lua   lua/lib/" instead of "lua/lib/lsp.lua",
        -- so the identifying token is always in the same column regardless of
        -- tree depth — much easier to skim than stock left-aligned paths.
        formatters = { file = { filename_first = true } },
        -- Follow symlinks for files/grep — without this, grepping a directory
        -- whose entries are symlinks silently skips them and returns 0 hits.
        sources = {
          files = { follow = true },
          grep  = { follow = true },
        },
      },
      terminal  = { enabled = true },
      bufdelete = { enabled = true },
      words     = { enabled = true },   -- LSP document_highlight under cursor
    },
    keys = {
      { "<Leader>ff", function() Snacks.picker.files() end,                   desc = "Find files" },
      { "<Leader>fg", function() Snacks.picker.grep() end,                    desc = "Live grep" },
      { "<Leader>fb", function() Snacks.picker.buffers() end,                 desc = "Buffers" },
      { "<Leader>fr", function() Snacks.picker.recent() end,                  desc = "Recent files" },
      { "<Leader>fc", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, desc = "Config files" },
      { "<Leader>fh", function() Snacks.picker.help() end,                    desc = "Help" },
      { "<Leader>fm", function() Snacks.picker.marks() end,                   desc = "Marks" },
      { "<Leader>fk", function() Snacks.picker.keymaps() end,                 desc = "Keymaps" },
      { "<Leader>sw", function() Snacks.picker.grep_word() end,               desc = "Grep word",  mode = { "n", "x" } },
      { "<Leader>sh", function() Snacks.picker.highlights() end,              desc = "Highlights" },
      { "<Leader>sc", function() Snacks.picker.command_history() end,         desc = "Command history" },
      { "<Leader>sn", function() Snacks.picker.notifications() end,           desc = "Notifications" },
      { "<Leader>cd", function() Snacks.picker.diagnostics_buffer() end,      desc = "Diagnostics (buffer)" },
      { "<Leader>cD", function() Snacks.picker.diagnostics() end,             desc = "Diagnostics (workspace)" },
      -- gd is the one pre-gr* classic we keep: nvim 0.11+ added `grr/gri/grt/gra/grn/gO`
      -- but not a definition verb in that family. Keeping `gd` here preserves
      -- universal muscle memory; `grr/gri/grt` (buffer-local overrides on
      -- LspAttach, see lib/lsp.lua) cover references / implementations / type.
      { "gd",         function() Snacks.picker.lsp_definitions() end,         desc = "LSP definitions" },
      { "<Leader>ss", function() Snacks.picker.lsp_symbols() end,             desc = "LSP symbols (buffer)" },
      { "<Leader>sS", function() Snacks.picker.lsp_workspace_symbols() end,   desc = "LSP symbols (workspace)" },
      -- git
      { "<Leader>gg", function() Snacks.lazygit() end,                        desc = "Lazygit (root)" },
      { "<Leader>gG", function() Snacks.lazygit({ cwd = vim.uv.cwd() }) end,  desc = "Lazygit (cwd)" },
      { "<Leader>gl", function() Snacks.lazygit.log_file() end,               desc = "Lazygit log (file)" },
      { "<Leader>gL", function() Snacks.lazygit.log() end,                    desc = "Lazygit log (all)" },
      { "<Leader>gf", function() Snacks.picker.git_files() end,               desc = "Git files" },
      { "<Leader>gs", function() Snacks.picker.git_status() end,              desc = "Git status" },
      { "<Leader>gc", function() Snacks.picker.git_log() end,                 desc = "Git commits" },
      { "<Leader>gb", function() Snacks.picker.git_branches() end,            desc = "Git branches" },
      -- terminal
      { "<C-/>",      function() Snacks.terminal() end,                        desc = "Terminal", mode = { "n", "t" } },
      { "<C-_>",      function() Snacks.terminal() end,                        desc = "Terminal (TTY alias)", mode = { "n", "t" } },
      -- buffers
      { "<Leader>bd", function() Snacks.bufdelete() end,                       desc = "Delete buffer" },
      { "<Leader>bD", function() Snacks.bufdelete.all() end,                   desc = "Delete all buffers" },
      { "<Leader>bo", function() Snacks.bufdelete.other() end,                 desc = "Delete other buffers" },
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
        { "<Leader>a", group = "ai (avante)" },
        { "<Leader>b", group = "buffer" },
        { "<Leader>c", group = "code (lsp)" },
        { "<Leader>d", group = "debug (dap)" },
        { "<Leader>f", group = "file / find" },
        { "<Leader>g", group = "git" },
        { "<Leader>gh", group = "git hunk" },
        { "<Leader>q", group = "quit" },
        { "<Leader>s", group = "search" },
        { "<Leader>t", group = "test" },
        { "<Leader>u", group = "ui / toggle" },
        { "<Leader>x", group = "trouble / diagnostics" },
        { "<Leader>z", group = "zk (notes)" },
      },
    },
  },
}
