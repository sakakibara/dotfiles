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
      custom_highlights = function(C)
        return {
          -- mantle is one shade darker than base (Normal bg).
          Folded = { bg = C.mantle },
        }
      end,
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
    priority = 900,
    lazy = false,  -- keys spec below would flip us to lazy; keep snacks eager (Snacks global needed by many)
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
      -- Snacks notifier replaces vim.notify at startup (lazy stub at
      -- snacks/init.lua:220-224 swaps in Snacks.notifier.notify on first
      -- call). That captures pre-noice startup errors into Snacks's own
      -- history, invisible to :messages and :NoiceAll. Noice is our
      -- single source of truth for notifications, so disable snacks's.
      notifier  = { enabled = false },
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
        -- Cycle the picker's search scope through three stops:
        --   1. project root (Lib.root: LSP / .git / lua marker)
        --   2. directory of the buffer that owned focus when the picker opened
        --   3. raw cwd
        -- Identical stops are deduped so the cycle always advances. Each
        -- press surfaces a one-line "scope: <path>" notification so the
        -- new working directory is obvious without inspecting the prompt.
        win = {
          input = {
            keys = {
              ["<A-c>"] = { "cycle_cwd", mode = { "n", "i" } },
            },
          },
        },
        actions = {
          cycle_cwd = function(p)
            local buf = p.input and p.input.filter and p.input.filter.current_buf
            local stops = { Lib.root(buf), Lib.root.buf(buf), vim.uv.cwd() }
            local seen, uniq = {}, {}
            for _, s in ipairs(stops) do
              local n = vim.fs.normalize(s)
              if not seen[n] then seen[n] = true; uniq[#uniq + 1] = n end
            end
            local cur, next_cwd = p:cwd(), uniq[1]
            for i, n in ipairs(uniq) do
              if n == cur then next_cwd = uniq[(i % #uniq) + 1]; break end
            end
            p:set_cwd(next_cwd)
            p:find()
            vim.notify(("scope: %s"):format(next_cwd), vim.log.levels.INFO)
          end,
        },
      },
      terminal  = { enabled = true },
      bufdelete = { enabled = true },
      words     = { enabled = true },   -- LSP document_highlight under cursor
    },
    keys = {
      { "<Leader>ff", function() Snacks.picker.files({ cwd = Lib.root() }) end,         desc = "Find files (root)" },
      { "<Leader>fF", function() Snacks.picker.files() end,                              desc = "Find files (cwd)" },
      { "<Leader>fg", function() Snacks.picker.grep({ cwd = Lib.root() }) end,           desc = "Live grep (root)" },
      { "<Leader>fG", function() Snacks.picker.grep() end,                               desc = "Live grep (cwd)" },
      { "<Leader>fb", function() Snacks.picker.buffers() end,                            desc = "Buffers" },
      { "<Leader>fr", function() Snacks.picker.recent() end,                             desc = "Recent files" },
      { "<Leader>fc", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, desc = "Config files" },
      { "<Leader>fh", function() Snacks.picker.help() end,                    desc = "Help" },
      { "<Leader>fm", function() Snacks.picker.marks() end,                   desc = "Marks" },
      { "<Leader>fk", function() Snacks.picker.keymaps() end,                 desc = "Keymaps" },
      { "<Leader>sw", function() Snacks.picker.grep_word({ cwd = Lib.root() }) end, desc = "Grep word (root)", mode = { "n", "x" } },
      { "<Leader>sW", function() Snacks.picker.grep_word() end,                     desc = "Grep word (cwd)",  mode = { "n", "x" } },
      { "<Leader>sh", function() Snacks.picker.highlights() end,              desc = "Highlights" },
      { "<Leader>sc", function() Snacks.picker.command_history() end,         desc = "Command history" },
      { "<Leader>sn", function() vim.cmd("Noice") end,                        desc = "Notifications" },
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
      { "<Leader>gg", function() Snacks.lazygit({ cwd = Lib.root.git() }) end,                desc = "Lazygit (git root)" },
      { "<Leader>gG", function() Snacks.lazygit({ cwd = vim.uv.cwd() }) end,                  desc = "Lazygit (cwd)" },
      { "<Leader>gl", function() Snacks.lazygit.log_file() end,               desc = "Lazygit log (file)" },
      { "<Leader>gL", function() Snacks.lazygit.log() end,                    desc = "Lazygit log (all)" },
      { "<Leader>gf", function() Snacks.picker.git_files() end,               desc = "Git files" },
      { "<Leader>gs", function() Snacks.picker.git_status() end,              desc = "Git status" },
      { "<Leader>gc", function() Snacks.picker.git_log() end,                 desc = "Git commits" },
      { "<Leader>gb", function() Snacks.git.blame_line() end,                 desc = "Git blame line" },
      { "<Leader>gB", function() Snacks.picker.git_branches() end,            desc = "Git branches" },
      { "<Leader>gw", function() Snacks.gitbrowse() end,                      desc = "Git browse (open)", mode = { "n", "x" } },
      { "<Leader>gy", function()
        Snacks.gitbrowse({ open = function(url) vim.fn.setreg("+", url) end })
      end, desc = "Git browse (yank URL)", mode = { "n", "x" } },
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
    event = "VeryLazy",
    opts = function()
      -- Byte-escaped nerd-font glyphs (PUA codepoints survive edits
      -- through tools that silently strip Private Use Area characters).
      local I = {
        ai       = "\xef\x95\x84", -- U+F544 robot
        buffer   = "\xef\x83\xaa", -- U+F0EA clipboard
        code     = "\xef\x84\xa1", -- U+F121 <>
        debug    = "\xef\x86\x88", -- U+F188 bug
        file     = "\xef\x85\x9b", -- U+F15B file
        find     = "\xef\x80\x82", -- U+F002 magnifier
        git      = "\xef\x87\x93", -- U+F1D3 git
        hunk     = "\xef\x91\x80", -- U+F440 file-diff
        org      = "\xef\x80\xad", -- U+F02D book
        quit     = "\xef\x80\x91", -- U+F011 power
        test     = "\xef\x83\x83", -- U+F0C3 flask
        ui       = "\xef\x80\x93", -- U+F013 gear
        window   = "\xef\x8b\x92", -- U+F2D2 window
        trouble  = "\xef\x81\xb1", -- U+F071 triangle-warning
        zk       = "\xef\x80\xae", -- U+F02E bookmark
        save     = "\xef\x83\x87", -- U+F0C7 floppy
        yank     = "\xef\x83\x85", -- U+F0C5 copy
        harpoon  = "\xef\x84\xbd", -- U+F13D anchor
        flash    = "\xef\x83\xa7", -- U+F0E7 bolt
        fold     = "\xef\x81\xb7", -- U+F077 chevron-up
        surround = "\xef\x84\x9c", -- U+F11C keyboard
        play     = "\xef\x81\x8b", -- U+F04B play
        eye      = "\xef\x81\xae", -- U+F06E eye
        plus     = "\xef\x81\xa7", -- U+F067 +
        minus    = "\xef\x81\xa8", -- U+F068 -
        close    = "\xef\x80\x8d", -- U+F00D X
        open     = "\xef\x81\xbc", -- U+F07C folder-open
        bell     = "\xef\x83\xb3", -- U+F0F3 bell
        help     = "\xef\x84\xa9", -- U+F129 info
        keyboard = "\xef\x84\x9c", -- U+F11C keyboard
        palette  = "\xef\x94\xbf", -- U+F53F palette
        pencil   = "\xef\x81\x80", -- U+F040 pencil
        arrow    = "\xef\x81\xa1", -- U+F061 arrow-right
        mobile   = "\xef\x84\x8b", -- U+F10B mobile
        terminal = "\xef\x84\xa0", -- U+F120 terminal
        pack     = "\xef\x91\xb6", -- U+F476 package
        resize   = "\xef\x83\xac", -- U+F0EC exchange
        trash    = "\xef\x87\xb8", -- U+F1F8 trash
        broom    = "\xef\x94\x9a", -- U+F51A broom
        check    = "\xef\x81\x86", -- U+F046 check-square
        list     = "\xef\x80\xba", -- U+F03A list
        paste    = "\xef\x83\xa2", -- U+F0E2 history (used as paste-back)
        link     = "\xef\x83\x81", -- U+F0C1 link
      }

      return {
        preset = "helix",
        icons = {
          -- User rules run BEFORE built-ins; first match wins. Patterns
          -- are case-insensitive substring (lua patterns), tested
          -- against the entry's `desc`.
          rules = {
            -- groups + general
            { pattern = "^save",          icon = I.save,    color = "green"  },
            { pattern = "^yank",          icon = I.yank,    color = "yellow" },
            { pattern = "yank entry",     icon = I.yank,    color = "yellow" },
            { pattern = "yank history",   icon = I.yank,    color = "yellow" },
            { pattern = "harpoon",        icon = I.harpoon, color = "azure"  },
            { pattern = "flash",          icon = I.flash,   color = "yellow" },
            { pattern = "fold",           icon = I.fold,    color = "grey"   },
            { pattern = "^peek",          icon = I.fold,    color = "grey"   },
            { pattern = "surround",       icon = I.surround, color = "purple" },
            { pattern = "grep",           icon = I.find,    color = "green"  },
            { pattern = "^run",           icon = I.play,    color = "green"  },
            { pattern = "^watch",         icon = I.eye,     color = "cyan"   },
            { pattern = "increment",      icon = I.plus,    color = "green"  },
            { pattern = "decrement",      icon = I.minus,   color = "red"    },
            { pattern = "^inc seq",       icon = I.plus,    color = "green"  },
            { pattern = "^dec seq",       icon = I.minus,   color = "red"    },
            -- DAP descs use "DAP" not "debug"; built-in rule misses them.
            { pattern = "^dap ",          icon = I.debug,   color = "red"    },
            { pattern = "step into",      icon = I.debug,   color = "red"    },
            { pattern = "step over",      icon = I.debug,   color = "red"    },
            { pattern = "step out",       icon = I.debug,   color = "red"    },
            { pattern = "breakpoint",     icon = I.debug,   color = "red"    },
            { pattern = "^continue",      icon = I.play,    color = "red"    },
            { pattern = "^terminate",     icon = I.close,   color = "red"    },
            { pattern = "^stop",          icon = I.close,   color = "red"    },
            { pattern = "^restart",       icon = I.play,    color = "yellow" },
            { pattern = "^eval",          icon = I.terminal, color = "cyan"  },
            -- plugin-specific descs without command-name auto-detect
            { pattern = "lazygit",        icon = I.git,     color = "orange" },
            { pattern = "flutter",        icon = I.mobile,  color = "blue"   },
            { pattern = "markdown",       icon = I.pencil,  color = "blue"   },
            { pattern = "metals",         icon = I.code,    color = "red"    },
            { pattern = "ansible",        icon = I.code,    color = "red"    },
            { pattern = "avante",         icon = I.ai,      color = "purple" },
            { pattern = "^pack ",         icon = I.pack,    color = "azure"  },
            -- LSP / code actions
            { pattern = "^goto",          icon = I.arrow,   color = "yellow" },
            { pattern = "definition",     icon = I.arrow,   color = "yellow" },
            { pattern = "organize",       icon = I.code,    color = "cyan"   },
            { pattern = "imports",        icon = I.code,    color = "cyan"   },
            { pattern = "^switch",        icon = I.resize,  color = "blue"   },
            -- color picker / swatches
            { pattern = "color picker",   icon = I.palette, color = "purple" },
            { pattern = "swatch",         icon = I.palette, color = "purple" },
            { pattern = "highlight",      icon = I.palette, color = "yellow" },
            -- pickers / panes
            { pattern = "scope picker",   icon = I.find,    color = "azure"  },
            { pattern = "path picker",    icon = I.find,    color = "azure"  },
            { pattern = "smart find",     icon = I.find,    color = "green"  },
            { pattern = "command history", icon = I.terminal, color = "cyan" },
            -- editing
            { pattern = "^trim ",         icon = I.broom,   color = "yellow" },
            { pattern = "^delete ",       icon = I.trash,   color = "red"    },
            { pattern = "^close ",        icon = I.close,   color = "red"    },
            { pattern = "^open ",         icon = I.open,    color = "yellow" },
            { pattern = "^put ",          icon = I.paste,   color = "yellow" },
            { pattern = "^select ",       icon = I.check,   color = "azure"  },
            -- notes / zk
            { pattern = "note",           icon = I.zk,      color = "purple" },
            { pattern = "backlinks",      icon = I.link,    color = "azure"  },
            { pattern = "outbound link",  icon = I.link,    color = "azure"  },
            { pattern = "insert link",    icon = I.link,    color = "azure"  },
            { pattern = "^index ",        icon = I.list,    color = "cyan"   },
            { pattern = "find by tags",   icon = I.find,    color = "purple" },
            -- misc
            { pattern = "keymap",         icon = I.keyboard, color = "azure" },
            { pattern = "^help",          icon = I.help,    color = "cyan"   },
            { pattern = "leave terminal", icon = I.terminal, color = "red"   },
            { pattern = "set filetype",   icon = I.file,    color = "cyan"   },
            { pattern = "^oil",           icon = I.open,    color = "yellow" },
            { pattern = "parent dir",     icon = I.open,    color = "yellow" },
            -- treesitter-textobjects motion
            { pattern = "next class",     icon = I.code,    color = "yellow" },
            { pattern = "prev class",     icon = I.code,    color = "yellow" },
            { pattern = "next function",  icon = I.code,    color = "yellow" },
            { pattern = "prev function",  icon = I.code,    color = "yellow" },
            { pattern = "reference",      icon = I.link,    color = "azure"  },
            -- diagnostic severity (built-in 'diagnostic' rule misses these)
            { pattern = "next error",     icon = I.trouble, color = "red"    },
            { pattern = "prev error",     icon = I.trouble, color = "red"    },
            { pattern = "next warning",   icon = I.trouble, color = "yellow" },
            { pattern = "prev warning",   icon = I.trouble, color = "yellow" },
            -- conjure eval-output jumps
            { pattern = "eval output",    icon = I.terminal, color = "cyan"  },
            -- DAP descs where "dap" isn't at the start
            { pattern = "dap server",     icon = I.debug,   color = "red"    },
            { pattern = "lua dap",        icon = I.debug,   color = "red"    },
            { pattern = "under dap",      icon = I.debug,   color = "red"    },
            -- task runners / list panes
            { pattern = "overseer",       icon = I.play,    color = "yellow" },
            { pattern = "quickfix",       icon = I.list,    color = "azure"  },
            { pattern = "loclist",        icon = I.list,    color = "azure"  },
            -- window operations missing the word "window"
            { pattern = "^resize",        icon = I.resize,  color = "blue"   },
            -- chezmoi / case / checkbox / cd
            { pattern = "chezmoi",        icon = I.ui,      color = "yellow" },
            { pattern = "checkbox",       icon = I.check,   color = "green"  },
            { pattern = "^case",          icon = I.pencil,  color = "yellow" },
            { pattern = "^cd ",           icon = I.open,    color = "yellow" },
          },
        },
        spec = {
          { "<Leader>a",  group = "ai (avante)",           icon = { icon = I.ai,      color = "purple" } },
          { "<Leader>b",  group = "buffer",                icon = { icon = I.buffer,  color = "cyan"   } },
          { "<Leader>c",  group = "code (lsp)",            icon = { icon = I.code,    color = "orange" } },
          { "<Leader>d",  group = "debug (dap)",           icon = { icon = I.debug,   color = "red"    } },
          { "<Leader>f",  group = "file / find",           icon = { icon = I.file,    color = "cyan"   } },
          { "<Leader>g",  group = "git",                   icon = { icon = I.git,     color = "orange" } },
          { "<Leader>gh", group = "git hunk",              icon = { icon = I.hunk,    color = "orange" } },
          { "<Leader>o",  group = "org (organ)",           icon = { icon = I.org,     color = "green"  } },
          { "<Leader>P",  group = "pack",                  icon = { icon = I.pack,    color = "azure"  } },
          { "<Leader>q",  group = "quit",                  icon = { icon = I.quit,    color = "red"    } },
          { "<Leader>s",  group = "search",                icon = { icon = I.find,    color = "green"  } },
          { "<Leader>t",  group = "test",                  icon = { icon = I.test,    color = "yellow" } },
          { "<Leader>u",  group = "ui / toggle",           icon = { icon = I.ui,      color = "cyan"   } },
          { "<Leader>w",  group = "window",                icon = { icon = I.window,  color = "blue"   } },
          { "<Leader>x",  group = "trouble / diagnostics", icon = { icon = I.trouble, color = "red"    } },
          { "<Leader>z",  group = "zk (notes)",            icon = { icon = I.zk,      color = "purple" } },
        },
      }
    end,
  },
}
