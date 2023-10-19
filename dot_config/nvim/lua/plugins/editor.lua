local utelescope = require("util.telescope")
local root_path = require("util.root").get
local parent_path = require("util.path").get_parent_path
return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      plugins = { spelling = true },
      defaults = {
        mode = { "n", "v" },
        ["<leader>b"] = { name = "+buffer" },
        ["<leader>c"] = { name = "+code" },
        ["<leader>f"] = { name = "+file" },
        ["<leader>g"] = { name = "+git" },
        ["<leader>gh"] = { name = "+hunks" },
        ["<leader>n"] = { name = "+notes" },
        ["<leader>r"] = { name = "+relative" },
        ["<leader>rf"] = { name = "+file" },
        ["<leader>rs"] = { name = "+search" },
        ["<leader>s"] = { name = "+search" },
        ["<leader>t"] = { name = "+toggle" },
        ["<leader>u"] = { name = "+ui" },
        ["["] = { name = "+prev" },
        ["]"] = { name = "+next" },
        ["gs"] = { name = "+surround" },
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.register(opts.defaults)
    end,
  },

  {
    "antoinemadec/FixCursorHold.nvim",
    event = "UIEnter",
    init = function()
      vim.g.cursorhold_updatetime = 100
    end,
  },

  {
    "linty-org/readline.nvim",
    keys = {
      {
        "<C-k>",
        function()
          require("readline").kill_line()
        end,
        desc = "Kill line",
        mode = "c",
      },
      {
        "<C-u>",
        function()
          require("readline").backward_kill_line()
        end,
        desc = "Backward kill line",
        mode = "c",
      },
      {
        "<M-d>",
        function()
          require("readline").kill_word()
        end,
        desc = "Kill word",
        mode = "c",
      },
      {
        "<M-BS>",
        function()
          require("readline").backward_kill_word()
        end,
        desc = "Backward kill word",
        mode = "c",
      },
      {
        "<C-w>",
        function()
          require("readline").unix_word_rubout()
        end,
        desc = "Unix word rubout",
        mode = "c",
      },
      { "<C-d>", "<delete>", desc = "Delete character", mode = "c" },
      { "<C-h>", "<bs>", desc = "Backward delete character", mode = "c" },
      {
        "<C-a>",
        function()
          require("readline").beginning_of_line()
        end,
        desc = "Beginning of line",
        mode = "c",
      },
      {
        "<C-e>",
        function()
          require("readline").end_of_line()
        end,
        desc = "End of line",
        mode = "c",
      },
      {
        "<M-f>",
        function()
          require("readline").forward_word()
        end,
        desc = "Forward word",
        mode = "c",
      },
      {
        "<M-b>",
        function()
          require("readline").backward_word()
        end,
        desc = "Backward word",
        mode = "c",
      },
      { "<C-f>", "<right>", desc = "Forward char", mode = "c" },
      { "<C-b>", "<left>", desc = "Backward char", mode = "c" },
      { "<C-n>", "<down>", desc = "Next line", mode = "c" },
      { "<C-p>", "<up>", desc = "Previous line", mode = "c" },
    },
  },

  {
    "chrishrb/gx.nvim",
    event = { "BufEnter" },
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      handlers = {
        plugin = true,
        github = true,
        brewfile = true,
        package_json = true,
        search = true,
      },
      handler_options = {
        search_engine = "duckduckgo",
      },
    },
  },

  {
    "echasnovski/mini.bufremove",
    keys = {
      {
        "<leader>bd",
        function()
          require("mini.bufremove").delete(0, false)
        end,
        desc = "Delete buffer",
      },
      {
        "<leader>bD",
        function()
          require("mini.bufremove").delete(0, true)
        end,
        desc = "Delete buffer (force)",
      },
    },
  },

  {
    "echasnovski/mini.splitjoin",
    event = "VeryLazy",
    opts = {
      mappings = {
        toggle = "gS",
      },
    },
  },

  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    opts = {
      mapping = { "jk" },
    },
  },

  {
    "monaqa/dial.nvim",
    keys = {
      {
        "<C-a>",
        function()
          return require("dial.map").inc_normal()
        end,
        expr = true,
        desc = "Increment",
      },
      {
        "<C-x>",
        function()
          return require("dial.map").dec_normal()
        end,
        expr = true,
        desc = "Decrement",
      },
      {
        "<C-a>",
        function()
          return require("dial.map").inc_visual()
        end,
        expr = true,
        desc = "Increment",
        mode = "v",
      },
      {
        "<C-x>",
        function()
          return require("dial.map").dec_visual()
        end,
        expr = true,
        desc = "Decrement",
        mode = "v",
      },
      {
        "g<C-a>",
        function()
          return require("dial.map").inc_gvisual()
        end,
        expr = true,
        desc = "Increment",
        mode = "v",
      },
      {
        "g<C-x>",
        function()
          return require("dial.map").dec_gvisual()
        end,
        expr = true,
        desc = "Decrement",
        mode = "v",
      },
    },
    config = function()
      local augend = require("dial.augend")
      require("dial.config").augends:register_group({
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias["%Y/%m/%d"],
          augend.date.alias["%Y-%m-%d"],
          augend.date.alias["%Y年%-m月%-d日"],
          augend.date.alias["%H:%M:%S"],
          augend.date.alias["%H:%M"],
          augend.constant.alias.bool,
          augend.semver.alias.semver,
        },
        visual = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias["%Y/%m/%d"],
          augend.date.alias["%Y-%m-%d"],
          augend.date.alias["%Y年%-m月%-d日"],
          augend.date.alias["%H:%M:%S"],
          augend.date.alias["%H:%M"],
          augend.constant.alias.bool,
          augend.constant.alias.alpha,
          augend.constant.alias.Alpha,
          augend.semver.alias.semver,
        },
      })
    end,
  },

  {
    "stevearc/overseer.nvim",
    cmd = {
      "OverseerOpen",
      "OverseerClose",
      "OverseerToggle",
      "Grep",
    },
    opts = {},
    config = function(_, opts)
      require("overseer").setup(opts)
      vim.api.nvim_create_user_command("Grep", function(params)
        local cmd, num_subs = vim.o.grepprg:gsub("%$%*", params.args)
        if num_subs == 0 then
          cmd = cmd .. " " .. params.args
        end
        local task = require("overseer").new_task({
          cmd = vim.fn.expandcmd(cmd),
          components = {
            {
              "on_output_quickfix",
              errorformat = vim.o.grepformat,
              open = not params.bang,
              open_height = 8,
              items_only = true,
            },
            { "on_complete_dispose", timeout = 30 },
            "default",
          },
        })
        task:start()
      end, { nargs = "*", bang = true, complete = "file" })
    end,
  },

  {
    "RRethy/vim-illuminate",
    event = "LazyFile",
    opts = {
      delay = 200,
      filetypes_denylist = {
        "csv",
        "tsv",
        "text",
        "oil",
      },
      large_file_cutoff = 10000,
    },
    config = function(_, opts)
      require("illuminate").configure(opts)
    end,
    keys = {
      {
        "]]",
        function()
          require("illuminate").goto_next_reference(false)
        end,
        desc = "Next reference",
      },
      {
        "[[",
        function()
          require("illuminate").goto_prev_reference(false)
        end,
        desc = "Prev reference",
      },
    },
  },

  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {
      modes = {
        search = {
          enabled = false,
        },
      },
    },
    keys = {
      {
        "s",
        mode = { "n", "o", "x" },
        function()
          require("flash").jump()
        end,
        desc = "Flash",
      },
      {
        "S",
        mode = { "n", "o", "x" },
        function()
          require("flash").treesitter()
        end,
        desc = "Flash treesitter",
      },
      {
        "r",
        mode = "o",
        function()
          require("flash").remote()
        end,
        desc = "Remote flash",
      },
      {
        "R",
        mode = { "o", "x" },
        function()
          require("flash").treesitter_search()
        end,
        desc = "Treesitter search",
      },
      {
        "<c-s>",
        mode = { "c" },
        function()
          require("flash").toggle()
        end,
        desc = "Toggle flash search",
      },
    },
  },

  {
    "nvim-telescope/telescope.nvim",
    optional = true,
    opts = function(_, opts)
      local function flash(prompt_bufnr)
        require("flash").jump({
          pattern = "^",
          label = { after = { 0, 0 } },
          search = {
            mode = "search",
            exclude = {
              function(win)
                return vim.bo[vim.api.nvim_win_get_buf(win)].filetype ~= "TelescopeResults"
              end,
            },
          },
          action = function(match)
            local picker = require("telescope.actions.state").get_current_picker(prompt_bufnr)
            picker:set_selection(match.pos[1] - 1)
          end,
        })
      end
      opts.defaults = vim.tbl_deep_extend("force", opts.defaults or {}, {
        mappings = { n = { s = flash }, i = { ["<c-s>"] = flash } },
      })
    end,
  },

  {
    "lewis6991/gitsigns.nvim",
    event = "LazyFile",
    opts = {
      signs = {
        add = { text = "▎" },
        change = { text = "▎" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "▎" },
        untracked = { text = "▎" },
      },
      on_attach = function(buffer)
        local gs = package.loaded.gitsigns

        local function map(mode, l, r, desc)
          vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
        end

        map("n", "]h", gs.next_hunk, "Next Hunk")
        map("n", "[h", gs.prev_hunk, "Prev Hunk")
        map({ "n", "v" }, "<leader>ghs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
        map({ "n", "v" }, "<leader>ghr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
        map("n", "<leader>ghS", gs.stage_buffer, "Stage Buffer")
        map("n", "<leader>ghu", gs.undo_stage_hunk, "Undo Stage Hunk")
        map("n", "<leader>ghR", gs.reset_buffer, "Reset Buffer")
        map("n", "<leader>ghp", gs.preview_hunk, "Preview Hunk")
        map("n", "<leader>ghb", function()
          gs.blame_line({ full = true })
        end, "Blame Line")
        map("n", "<leader>ghd", gs.diffthis, "Diff This")
        map("n", "<leader>ghD", function()
          gs.diffthis("~")
        end, "Diff This ~")
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
      end,
    },
  },

  {
    "AckslD/muren.nvim",
    cmd = {
      "MurenToggle",
      "MurenOpen",
      "MurenClose",
      "MurenFresh",
      "MurenUnique",
    },
    opts = {},
  },

  {
    "stefandtw/quickfix-reflector.vim",
    ft = "qf",
    init = function()
      vim.g.qf_join_changes = 1
    end,
  },

  {
    "stevearc/qf_helper.nvim",
    ft = "qf",
    cmd = {
      "QNext",
      "QPrev",
      "QFNext",
      "QFPrev",
      "LLNext",
      "LLPrev",
      "QFOpen",
      "LLOpen",
      "QFToggle",
      "LLToggle",
      "Cclear",
      "Lclear",
      "Keep",
      "Reject",
    },
    keys = {
      { "<leader>xl", "<cmd>LLToggle<cr>", desc = "Location list" },
      { "<leader>xq", "<cmd>QFToggle<cr>", desc = "Quickfix list" },
    },
    opts = {
      quickfix = {
        track_location = false,
      },
      loclist = {
        track_location = false,
      },
    },
  },

  {
    "folke/trouble.nvim",
    cmd = { "TroubleToggle", "Trouble" },
    opts = { use_diagnostic_signs = true },
    keys = {
      { "<leader>xx", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Document diagnostics (trouble)" },
      { "<leader>xX", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Workspace diagnostics (trouble)" },
      { "<leader>xL", "<cmd>TroubleToggle loclist<cr>", desc = "Location list (trouble)" },
      { "<leader>xQ", "<cmd>TroubleToggle quickfix<cr>", desc = "Quickfix list (trouble)" },
      {
        "[q",
        function()
          if require("trouble").is_open() then
            require("trouble").previous({ skip_groups = true, jump = true })
          elseif require("util.plugin").has("qf_helper.nvim") then
            vim.cmd("QPrev")
          else
            local ok, err = pcall(vim.cmd.cprev)
            if not ok then
              vim.notify(err, vim.log.levels.ERROR)
            end
          end
        end,
        desc = "Previous trouble/quickfix item",
      },
      {
        "]q",
        function()
          if require("trouble").is_open() then
            require("trouble").next({ skip_groups = true, jump = true })
          elseif require("util.plugin").has("qf_helper.nvim") then
            vim.cmd("QNext")
          else
            local ok, err = pcall(vim.cmd.cnext)
            if not ok then
              vim.notify(err, vim.log.levels.ERROR)
            end
          end
        end,
        desc = "Next trouble/quickfix item",
      },
    },
  },

  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
      },
    },
    cmd = "Telescope",
    version = false,
    keys = {
      { "<leader><space>", utelescope.func("files", { cwd = root_path }), desc = "Files" },
      { "<leader>sb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>sf", utelescope.func("files", { cwd = root_path }), desc = "Files" },
      { "<leader>sF", utelescope.func("files", { cwd = false }), desc = "Files (cwd)" },
      { "<leader>rsf", utelescope.func("find_files", { cwd = parent_path }), desc = "Find files (relative)" },
      { "<leader>ff", utelescope.func("files", { cwd = root_path }), desc = "Files" },
      { "<leader>fF", utelescope.func("files", { cwd = false }), desc = "Files (cwd)" },
      { "<leader>rff", utelescope.func("find_files", { cwd = parent_path }), desc = "Find files (relative)" },
      { "<leader>sg", utelescope.func("live_grep", { cwd = root_path }), desc = "Grep" },
      { "<leader>sG", utelescope.func("live_grep", { cwd = false }), desc = "Grep (cwd)" },
      { "<leader>rsg", utelescope.func("live_grep", { cwd = parent_path }), desc = "Grep (relative)" },
      { "<leader>sw", utelescope.func("grep_string", { cwd = root_path, word_match = "-w" }), desc = "Word" },
      { "<leader>sW", utelescope.func("grep_string", { cwd = false, word_match = "-w" }), desc = "Word (cwd)" },
      {
        "<leader>rsw",
        utelescope.func("grep_string", { cwd = parent_path, word_match = "-w" }),
        desc = "Word (relative)",
      },
      { "<leader>sw", utelescope.func("grep_string", { cwd = root_path }), mode = "v", desc = "Selection" },
      { "<leader>sW", utelescope.func("grep_string", { cwd = false }), mode = "v", desc = "Selection (cwd)" },
      {
        "<leader>srw",
        utelescope.func("grep_string", { cwd = parent_path }),
        mode = "v",
        desc = "Selection (relative)",
      },
      { '<leader>s"', "<cmd>Telescope registers<cr>", desc = "Registers" },
      { "<leader>ss", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Fuzzy search buffer lines" },
      { "<leader>sc", "<cmd>Telescope command_history<cr>", desc = "Command history" },
      { "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Help tags" },
      { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
      { "<leader>tt", utelescope.func("colorscheme", { enable_preview = true }), desc = "Colorschemes" },
      { "<leader>:", "<cmd>Telescope commands<cr>", desc = "Commands" },
      { "<leader>'", "<cmd>Telescope resume<cr>", desc = "Resume" },
    },
    opts = {
      defaults = {
        preview = { hide_on_startup = true },
        prompt_prefix = "❯ ",
        selection_caret = " ",
        mappings = {
          i = {
            ["<c-t>"] = function(...)
              return require("trouble.providers.telescope").open_with_trouble(...)
            end,
            ["<a-i>"] = function()
              local action_state = require("telescope.actions.state")
              local line = action_state.get_current_line()
              utelescope.func("find_files", { no_ignore = true, default_text = line })()
            end,
            ["<a-h>"] = function()
              local action_state = require("telescope.actions.state")
              local line = action_state.get_current_line()
              utelescope.func("find_files", { hidden = true, default_text = line })()
            end,
            ["<C-Down>"] = function(...)
              return require("telescope.actions").cycle_history_next(...)
            end,
            ["<C-Up>"] = function(...)
              return require("telescope.actions").cycle_history_prev(...)
            end,
            ["<C-n>"] = function(...)
              return require("telescope.actions").move_selection_next(...)
            end,
            ["<C-p>"] = function(...)
              return require("telescope.actions").move_selection_previous(...)
            end,
            ["<M-p>"] = function(...)
              return require("telescope.actions.layout").toggle_preview(...)
            end,
            ["<C-k>"] = function()
              require("readline").kill_line()
            end,
            ["<C-u>"] = function()
              require("readline").backward_kill_line()
            end,
            ["<M-d>"] = function()
              require("readline").kill_word()
            end,
            ["<M-BS>"] = function()
              require("readline").backward_kill_word()
            end,
            ["<C-w>"] = function()
              require("readline").unix_word_rubout()
            end,
            ["<C-d>"] = { "<delete>", type = "command" },
            ["<C-h>"] = { "<bs>", type = "command" },
            ["<C-a>"] = function()
              require("readline").beginning_of_line()
            end,
            ["<C-e>"] = function()
              require("readline").end_of_line()
            end,
            ["<M-f>"] = function()
              require("readline").forward_word()
            end,
            ["<M-b>"] = function()
              require("readline").backward_word()
            end,
            ["<C-f>"] = { "<right>", type = "command" },
            ["<C-b>"] = { "<left>", type = "command" },
          },
          n = {
            ["-"] = function(...)
              return require("telescope.actions").nop(...)
            end,
            ["q"] = function(...)
              return require("telescope.actions").close(...)
            end,
            ["<M-p>"] = function(...)
              return require("telescope.actions.layout").toggle_preview(...)
            end,
          },
        },
      },
      extensions = {
        file_browser = {
          theme = "ivy",
          mappings = {
            i = {
              ["<Tab>"] = function(...)
                return require("telescope.actions").select_default(...)
              end,
            },
          },
        },
        fzf = {
          fuzzy = true,
          override_generic_sorter = true,
          override_file_sorter = true,
          case_mode = "smart_case",
        },
        undo = {
          use_delta = false,
          use_custom_command = nil,
          side_by_side = false,
          diff_context_lines = vim.o.scrolloff,
          entry_format = "state #$ID, $STAT, $TIME",
          time_format = "",
          mappings = {
            i = {
              ["<C-y>"] = function(...)
                return require("telescope-undo.actions").yank_additions(...)
              end,
              ["<M-y>"] = function(...)
                return require("telescope-undo.actions").yank_deletions(...)
              end,
              ["<CR>"] = function(...)
                return require("telescope-undo.actions").restore(...)
              end,
            },
          },
        },
        frecency = {
          auto_validate = false,
          show_unindexed = false,
          use_sqlite = false,
        },
        zoxide = {
          list_command = "zoxide query -ls --all",
          mappings = {
            default = {
              action = function(selection)
                vim.cmd.cd(selection.path)
                require("oil").open(selection.path)
              end,
            },
            ["<C-s>"] = {
              before_action = function()
                require("oil")
              end,
            },
            ["<C-v>"] = {
              before_action = function()
                require("oil")
              end,
            },
            ["<C-e>"] = {
              before_action = function()
                require("oil")
              end,
            },
          },
        },
      },
    },
    config = function(_, opts)
      require("telescope").setup(opts)
      require("telescope").load_extension("fzf")
    end,
  },
  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    keys = {
      { "<leader>sa", utelescope.func("live_grep_args", { cwd = root_path }), desc = "Grep with args" },
      { "<leader>sA", utelescope.func("live_grep_args", { cwd = false }), desc = "Grep with args (cwd)" },
      { "<leader>rsa", utelescope.func("live_grep_args", { cwd = parent_path }), desc = "Grep with args (relative)" },
    },
    config = function()
      require("telescope").load_extension("live_grep_args")
    end,
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    keys = {
      {
        "<leader>fb",
        utelescope.func("file_browser", { cwd = parent_path }),
        desc = "File browser",
      },
    },
    config = function()
      require("telescope").load_extension("file_browser")
    end,
  },
  {
    "nvim-telescope/telescope-frecency.nvim",
    event = { "VeryLazy" },
    keys = {
      { "<leader>sr", "<cmd>Telescope frecency<cr>", desc = "Oldfiles" },
      { "<leader>fr", "<cmd>Telescope frecency<cr>", desc = "Oldfiles" },
    },
    config = function()
      require("telescope").load_extension("frecency")
    end,
  },
  {
    "debugloop/telescope-undo.nvim",
    keys = {
      { "<leader>su", "<cmd>Telescope undo<cr>", desc = "Undo history" },
    },
    config = function()
      require("telescope").load_extension("undo")
    end,
  },
  {
    "jvgrootveld/telescope-zoxide",
    keys = {
      { "<leader>sz", "<cmd>Telescope zoxide list<cr>", desc = "Zoxide" },
    },
    config = function()
      require("telescope").load_extension("zoxide")
    end,
  },

  {
    "mickael-menu/zk-nvim",
    keys = {
      { "<leader>nn", "<cmd>ZkNew { title = vim.fn.input('Title: ') }<cr>", desc = "Create zk note" },
      {
        "<leader>nj",
        "<cmd>ZkNew { dir = os.getenv('ZK_NOTEBOOK_DIR') .. require('util.path').sep .. 'journal', group = 'journal' }<cr>",
        desc = "Create zk journal note",
      },
      { "<leader>nf", "<cmd>ZkNotes { sort = { 'modified' } }<cr>", desc = "Open zk note" },
      { "<leader>ns", "<cmd>ZkGrep<cr>", desc = "Search zk note" },
      { "<leader>nt", "<cmd>ZkTags<cr>", desc = "Open zk note by tags" },
      {
        "<leader>no",
        "<cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search : ') } }<cr>",
        desc = "Open zk note by tags",
      },
      { "<leader>nr", "<cmd>ZkIndex<cr>", desc = "Refresh zk index" },
    },
    opts = {
      picker = "select",
      lsp = {
        config = {
          cmd = { "zk", "lsp" },
          name = "zk",
        },
        auto_attach = {
          enabled = true,
          filetypes = { "markdown" },
        },
      },
    },
    config = function(_, opts)
      require("zk").setup(opts)

      local function grep_notes()
        local collection = {}
        local list_opts = { select = { "title", "path", "absPath" } }
        require("zk.api").list(vim.env.ZK_NOTEBOOK_DIR, list_opts, function(_, notes)
          for _, note in ipairs(notes) do
            collection[note.absPath] = note.title or note.path
          end
        end)
        local options = vim.tbl_deep_extend("force", {
          prompt_title = "Notes",
          search_dirs = { vim.env.ZK_NOTEBOOK_DIR },
          disable_coordinates = true,
          path_display = function(_, path)
            return collection[path]
          end,
        }, opts or {})
        require("telescope.builtin").live_grep(options)
      end

      vim.api.nvim_create_user_command("ZkGrep", grep_notes, {})
    end,
  },

  {
    "tpope/vim-eunuch",
    cmd = {
      "Remove",
      "Delete",
      "Move",
      "Rename",
      "Copy",
      "Duplicate",
      "Chmod",
      "Mkdir",
      "Cfind",
      "Clocate",
      "Lfind",
      "Llocate",
      "Wall",
      "SudoWrite",
      "SudoEdit",
    },
  },
}
