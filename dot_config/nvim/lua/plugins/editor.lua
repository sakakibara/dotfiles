local utelescope = require("util.telescope")
local upath = require("util.path")
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
          local bd = require("mini.bufremove").delete
          if vim.bo.modified then
            local choice = vim.fn.confirm(("Save changes to %q?"):format(vim.fn.bufname()), "&Yes\n&No\n&Cancel")
            if choice == 1 then
              vim.cmd.write()
              bd(0)
            elseif choice == 2 then
              bd(0, true)
            end
          else
            bd(0)
          end
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
      {
        "<leader>bw",
        function()
          local bw = require("mini.bufremove").wipeout
          if vim.bo.modified then
            local choice = vim.fn.confirm(("Save changes to %q?"):format(vim.fn.bufname()), "&Yes\n&No\n&Cancel")
            if choice == 1 then
              vim.cmd.write()
              bw(0)
            elseif choice == 2 then
              bw(0, true)
            end
          else
            bw(0)
          end
        end,
        desc = "Wipeout buffer",
      },
      {
        "<leader>bW",
        function()
          require("mini.bufremove").wipeout(0, true)
        end,
        desc = "Wipeout buffer (force)",
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
    "echasnovski/mini.trailspace",
    event = "LazyFile",
    keys = {
      {
        "<leader>edt",
        function()
          require("mini.trailspace").trim()
        end,
        desc = "Trim trailing whitespace",
      },
      {
        "<leader>edl",
        function()
          require("mini.trailspace").trim_last_lines()
        end,
        desc = "Trim trailing empty lines",
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
      { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>ff", utelescope.func("files", { cwd = root_path }), desc = "Files" },
      { "<leader>fF", utelescope.func("find_files", { cwd = parent_path }), desc = "Find files (relative)" },
      { "<leader>fg", utelescope.func("live_grep", { cwd = root_path }), desc = "Grep" },
      { "<leader>fG", utelescope.func("live_grep", { cwd = parent_path }), desc = "Grep (relative)" },
      { "<leader>fw", utelescope.func("grep_string", { cwd = root_path, word_match = "-w" }), desc = "Word" },
      {
        "<leader>fW",
        utelescope.func("grep_string", { cwd = parent_path, word_match = "-w" }),
        desc = "Word (relative)",
      },
      { "<leader>fw", utelescope.func("grep_string", { cwd = root_path }), mode = "v", desc = "Selection" },
      {
        "<leader>fW",
        utelescope.func("grep_string", { cwd = parent_path }),
        mode = "v",
        desc = "Selection (relative)",
      },
      {
        "<leader>fs",
        function()
          require("telescope.builtin").lsp_document_symbols({
            symbols = require("util.filter").get_kind_filter(),
          })
        end,
        desc = "Goto symbol",
      },
      {
        "<leader>fS",
        function()
          require("telescope.builtin").lsp_dynamic_workspace_symbols({
            symbols = require("util.filter").get_kind_filter(),
          })
        end,
        desc = "Goto symbol (workspace)",
      },
      { '<leader>"', "<cmd>Telescope registers<cr>", desc = "Registers" },
      { "<leader>ss", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Fuzzy search buffer lines" },
      { "<leader>:", "<cmd>Telescope command_history<cr>", desc = "Command history" },
      { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
      { "<leader>sh", "<cmd>Telescope help_tags<cr>", desc = "Help tags" },
      { "<leader>sk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
      { "<leader>tt", utelescope.func("colorscheme", { enable_preview = true }), desc = "Colorschemes" },
      { "<leader>;", "<cmd>Telescope commands<cr>", desc = "Commands" },
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
        live_grep_args = {
          auto_quoting = true,
          mappings = {
            i = {
              ["<M-q>"] = function(...)
                return require("telescope-live-grep-args.actions").quote_prompt()(...)
              end,
            },
          },
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
      { "<leader>fa", utelescope.func("live_grep_args", { cwd = root_path }), desc = "Grep with args" },
      { "<leader>fA", utelescope.func("live_grep_args", { cwd = parent_path }), desc = "Grep with args (relative)" },
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
      {
        "<leader>nn",
        function()
          vim.ui.input({ prompt = "Title:" }, function(input)
            if input then
              require("zk").new({ title = input })
            end
          end)
        end,
        desc = "Create zk note",
      },
      {
        "<leader>nj",
        function()
          vim.ui.select({ "today", "yesterday", "tomorrow" }, {
            prompt = "Open journal:",
          }, function(choice)
            if choice then
              require("zk").new({
                date = choice == "today" and false or choice,
                dir = vim.env.ZK_NOTEBOOK_DIR .. upath.sep .. "journal",
                group = "journal",
              })
            end
          end)
        end,
        desc = "Create zk journal note",
      },
      { "<leader>nf", "<cmd>ZkNotes { sort = { 'modified' } }<cr>", desc = "Open zk note" },
      { "<leader>ns", "<cmd>ZkGrep<cr>", desc = "Search zk note" },
      { "<leader>nt", "<cmd>ZkTags<cr>", desc = "Open zk note by tags" },
      {
        "<leader>no",
        function()
          vim.ui.input({ prompt = "Search:" }, function(input)
            if input then
              require("zk").edit({ sort = { "modified" }, match = { input } })
            end
          end)
        end,
        desc = "Search and open zk notes",
      },
      { "<leader>nl", "<cmd>ZkLinks<cr>", desc = "Open zk links" },
      { "<leader>nb", "<cmd>ZkBacklinks<cr>", desc = "Open zk backlinks" },
      { "<leader>nr", "<cmd>ZkIndex<cr>", desc = "Refresh zk index" },
    },
    opts = {
      picker = "telescope",
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

  {
    "johmsalas/text-case.nvim",
    lazy = false,
    keys = {
      {
        "geu",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_upper_case")
        end,
        desc = "To upper case",
      },
      {
        "gel",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_lower_case")
        end,
        desc = "To lower case",
      },
      {
        "ges",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_snake_case")
        end,
        desc = "To snake case",
      },
      {
        "ged",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_dash_case")
        end,
        desc = "To dash case",
      },
      {
        "gen",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_constant_case")
        end,
        desc = "To constant case",
      },
      {
        "ged",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_dot_case")
        end,
        desc = "To dot case",
      },
      {
        "gea",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_phrase_case")
        end,
        desc = "To phrase case",
      },
      {
        "gec",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_camel_case")
        end,
        desc = "To camel case",
      },
      {
        "gep",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_pascal_case")
        end,
        desc = "To pascal case",
      },
      {
        "get",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_title_case")
        end,
        desc = "To title case",
      },
      {
        "gef",
        mode = { "n", "x" },
        function()
          require("textcase").current_word("to_path_case")
        end,
        desc = "To path case",
      },
      {
        "geU",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_upper_case")
        end,
        desc = "LSP rename to upper case",
      },
      {
        "geL",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_lower_case")
        end,
        desc = "LSP rename to lower case",
      },
      {
        "geS",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_snake_case")
        end,
        desc = "LSP rename to snake case",
      },
      {
        "geD",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_dash_case")
        end,
        desc = "LSP rename to dash case",
      },
      {
        "geN",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_constant_case")
        end,
        desc = "LSP rename to constant case",
      },
      {
        "geD",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_dot_case")
        end,
        desc = "LSP rename to dot case",
      },
      {
        "geA",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_phrase_case")
        end,
        desc = "LSP rename to phrase case",
      },
      {
        "geC",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_camel_case")
        end,
        desc = "LSP rename to camel case",
      },
      {
        "geP",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_pascal_case")
        end,
        desc = "LSP rename to pascal case",
      },
      {
        "geT",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_title_case")
        end,
        desc = "LSP rename to title case",
      },
      {
        "geF",
        mode = { "n", "x" },
        function()
          require("textcase").lsp_rename("to_path_case")
        end,
        desc = "LSP rename to upper case",
      },
      {
        "gEu",
        function()
          require("textcase").operator("to_upper_case")
        end,
        desc = "To upper case (operator)",
      },
      {
        "gEl",
        function()
          require("textcase").operator("to_lower_case")
        end,
        desc = "To lower case (operator)",
      },
      {
        "gEs",
        function()
          require("textcase").operator("to_snake_case")
        end,
        desc = "To snake case (operator)",
      },
      {
        "gEd",
        function()
          require("textcase").operator("to_dash_case")
        end,
        desc = "To dash case (operator)",
      },
      {
        "gEn",
        function()
          require("textcase").operator("to_constant_case")
        end,
        desc = "To constant case (operator)",
      },
      {
        "gEd",
        function()
          require("textcase").operator("to_dot_case")
        end,
        desc = "To dot case (operator)",
      },
      {
        "gEa",
        function()
          require("textcase").operator("to_phrase_case")
        end,
        desc = "To phrase case (operator)",
      },
      {
        "gEc",
        function()
          require("textcase").operator("to_camel_case")
        end,
        desc = "To camel case (operator)",
      },
      {
        "gEp",
        function()
          require("textcase").operator("to_pascal_case")
        end,
        desc = "To pascal case (operator)",
      },
      {
        "gEt",
        function()
          require("textcase").operator("to_title_case")
        end,
        desc = "To title case (operator)",
      },
      {
        "gEf",
        function()
          require("textcase").operator("to_path_case")
        end,
        desc = "To path case (operator)",
      },
    },
  },
}
