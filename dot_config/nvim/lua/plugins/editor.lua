return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      plugins = { spelling = true },
      defaults = {
        mode = { "n", "v" },
        ["<Leader>b"] = { name = "+Buffer" },
        ["<Leader>c"] = { name = "+Code" },
        ["<Leader>d"] = { name = "+Debug" },
        ["<Leader>da"] = { name = "+Adapters" },
        ["<Leader>e"] = { name = "+Editor" },
        ["<Leader>f"] = { name = "+File" },
        ["<Leader>fc"] = { name = "+Config" },
        ["<Leader>fp"] = { name = "+Parent" },
        ["<Leader>g"] = { name = "+Git" },
        ["<Leader>n"] = { name = "+Notes" },
        ["<Leader>o"] = { name = "+Option" },
        ["<Leader>q"] = { name = "+Quit" },
        ["<Leader>r"] = { name = "+REST" },
        ["<Leader>s"] = { name = "+Search" },
        ["<Leader>t"] = { name = "+Test" },
        ["<Leader>u"] = { name = "+UI" },
        ["<Leader>up"] = { name = "+Parent" },
        ["<Leader>ut"] = { name = "+Terminal" },
        ["["] = { name = "+Prev" },
        ["]"] = { name = "+Next" },
        ["gs"] = { name = "+Surround" },
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
    dependencies = { "nvim-lua/plenary.nvim" },
    cmd = { "Browse" },
    keys = {
      { "gx", "<Cmd>Browse<CR>", mode = { "n", "x" } },
    },
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
        "<Leader>bd",
        function()
          local bd = require("mini.bufremove").delete
          if vim.bo.modified then
            local bufname = vim.fn.bufname()
            local choice = vim.fn.confirm(
              ("Save changes to %q?"):format(bufname ~= "" and bufname or "[No Name]"),
              "&Yes\n&No\n&Cancel"
            )
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
        "<Leader>bD",
        function()
          require("mini.bufremove").delete(0, true)
        end,
        desc = "Delete buffer (force)",
      },
      {
        "<Leader>bw",
        function()
          local bw = require("mini.bufremove").wipeout
          if vim.bo.modified then
            local bufname = vim.fn.bufname()
            local choice = vim.fn.confirm(
              ("Save changes to %q?"):format(bufname ~= "" and bufname or "[No Name]"),
              "&Yes\n&No\n&Cancel"
            )
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
        "<Leader>bW",
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
    keys = {
      {
        "<Leader>ew",
        function()
          require("mini.trailspace").trim()
        end,
        desc = "Trim trailing whitespace",
      },
      {
        "<Leader>eW",
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
    "stevearc/overseer.nvim",
    event = "LazyFile",
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
    enabled = false,
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
        "<C-s>",
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
        mappings = { n = { s = flash }, i = { ["<C-s>"] = flash } },
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

        map({ "n", "v" }, "<Leader>gh", "", "Hunks")
        map("n", "]h", gs.next_hunk, "Next Hunk")
        map("n", "[h", gs.prev_hunk, "Prev Hunk")
        map({ "n", "v" }, "<Leader>ghs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
        map({ "n", "v" }, "<Leader>ghr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
        map("n", "<Leader>ghS", gs.stage_buffer, "Stage Buffer")
        map("n", "<Leader>ghu", gs.undo_stage_hunk, "Undo Stage Hunk")
        map("n", "<Leader>ghR", gs.reset_buffer, "Reset Buffer")
        map("n", "<Leader>ghp", gs.preview_hunk, "Preview Hunk")
        map("n", "<Leader>ghb", function()
          gs.blame_line({ full = true })
        end, "Blame Line")
        map("n", "<Leader>ghd", gs.diffthis, "Diff This")
        map("n", "<Leader>ghD", function()
          gs.diffthis("~")
        end, "Diff This ~")
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
      end,
    },
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
      { "<Leader>el", "<Cmd>LLToggle<CR>", desc = "Location list" },
      { "<Leader>eq", "<Cmd>QFToggle<CR>", desc = "Quickfix list" },
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
    cmd = { "Trouble" },
    keys = {
      { "<Leader>ed", "<Cmd>Trouble diagnostics toggle<CR>", desc = "Diagnostics (trouble)" },
      { "<Leader>eD", "<Cmd>Trouble diagnostics toggle filter.buf=0<CR>", desc = "Buffer diagnostics (trouble)" },
      { "<Leader>cs", "<Cmd>Trouble symbols toggle focus=false<CR>", desc = "Symbols (trouble)" },
      {
        "<Leader>cS",
        "<Cmd>Trouble lsp toggle focus=false win.position=right<CR>",
        desc = "LSP references/definitions/... (trouble)",
      },
      { "<Leader>eL", "<Cmd>Trouble loclist toggle<CR>", desc = "Location list (trouble)" },
      { "<Leader>eQ", "<Cmd>Trouble qflist toggle<CR>", desc = "Quickfix list (trouble)" },
      {
        "[q",
        function()
          if require("trouble").is_open() then
            require("trouble").previous({ skip_groups = true, jump = true })
          elseif Util.plugin.has("qf_helper.nvim") then
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
          elseif Util.plugin.has("qf_helper.nvim") then
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
    opts = {},
  },

  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        enabled = vim.fn.executable("make") == 1,
        config = function()
          Util.plugin.on_load("telescope.nvim", function()
            require("telescope").load_extension("fzf")
          end)
        end,
      },
    },
    cmd = "Telescope",
    version = false,
    keys = {
      { "<Leader><Space>", Util.telescope.run("files", { cwd = Util.root.get }), desc = "Files" },
      { "<Leader>bb", "<Cmd>Telescope buffers<CR>", desc = "Buffers" },
      { "<Leader>ff", Util.telescope.run("files", { cwd = Util.root.get }), desc = "Files" },
      { "<Leader>fF", Util.telescope.run("files", { cwd = vim.uv.cwd }), desc = "Files (cwd)" },
      {
        "<Leader>fpf",
        Util.telescope.run("find_files", { cwd = Util.path.get_parent_path }),
        desc = "Find files (parent)",
      },
      { "<Leader>fg", Util.telescope.run("live_grep", { cwd = Util.root.get }), desc = "Grep" },
      { "<Leader>fG", Util.telescope.run("live_grep", { cwd = vim.uv.cwd }), desc = "Grep (cwd)" },
      { "<Leader>fpg", Util.telescope.run("live_grep", { cwd = Util.path.get_parent_path }), desc = "Grep (parent)" },
      {
        "<Leader>ft",
        function()
          require("telescope.builtin").filetypes()
        end,
        desc = "Filetypes",
      },
      { "<Leader>fw", Util.telescope.run("grep_string", { cwd = Util.root.get, word_match = "-w" }), desc = "Word" },
      {
        "<Leader>fW",
        Util.telescope.run("grep_string", { cwd = vim.uv.cwd, word_match = "-w" }),
        desc = "Word (cwd)",
      },
      {
        "<Leader>fpw",
        Util.telescope.run("grep_string", { cwd = Util.path.get_parent_path, word_match = "-w" }),
        desc = "Word (parent)",
      },
      { "<Leader>fw", Util.telescope.run("grep_string", { cwd = Util.root.get }), mode = "v", desc = "Selection" },
      { "<Leader>fW", Util.telescope.run("grep_string", { cwd = vim.uv.cwd }), mode = "v", desc = "Selection (cwd)" },
      {
        "<Leader>fpw",
        Util.telescope.run("grep_string", { cwd = Util.path.get_parent_path }),
        mode = "v",
        desc = "Selection (parent)",
      },
      {
        "<Leader>fs",
        function()
          require("telescope.builtin").lsp_document_symbols({
            symbols = Util.telescope.get_kind_filter(),
          })
        end,
        desc = "Goto symbol",
      },
      {
        "<Leader>fS",
        function()
          require("telescope.builtin").lsp_dynamic_workspace_symbols({
            symbols = Util.telescope.get_kind_filter(),
          })
        end,
        desc = "Goto symbol (workspace)",
      },
      { "<Leader>fcf", Util.telescope.run("files", { cwd = vim.fn.stdpath("config") }), desc = "Files (config)" },
      { "<Leader>fcg", Util.telescope.run("live_grep", { cwd = vim.fn.stdpath("config") }), desc = "Grep (config)" },
      {
        "<Leader>fcw",
        Util.telescope.run("grep_string", { cwd = vim.fn.stdpath("config"), word_match = "-w" }),
        desc = "Word (config)",
      },
      {
        "<Leader>fcw",
        Util.telescope.run("grep_string", { cwd = vim.fn.stdpath("config") }),
        mode = "v",
        desc = "Selection (config)",
      },
      { '<Leader>"', "<Cmd>Telescope registers<CR>", desc = "Registers" },
      { "<Leader>ss", "<Cmd>Telescope current_buffer_fuzzy_find<CR>", desc = "Fuzzy search buffer lines" },
      { "<Leader>:", "<Cmd>Telescope command_history<CR>", desc = "Command history" },
      { "<Leader>/", "<Cmd>Telescope search_history<CR>", desc = "Search history" },
      { "<Leader>`", "<Cmd>Telescope marks<CR>", desc = "Marks" },
      { "<Leader>sa", "<Cmd>Telescope autocommands<CR>", desc = "Auto commands" },
      { "<Leader>sh", "<Cmd>Telescope help_tags<CR>", desc = "Help tags" },
      { "<Leader>sH", "<Cmd>Telescope highlights<CR>", desc = "Highlights" },
      { "<Leader>sj", "<Cmd>Telescope jumplist<CR>", desc = "Jump list" },
      { "<Leader>sk", "<Cmd>Telescope keymaps<CR>", desc = "Keymaps" },
      { "<Leader>sl", "<Cmd>Telescope loclist<CR>", desc = "Location list" },
      { "<Leader>sq", "<Cmd>Telescope quickfix<CR>", desc = "Quickfix list" },
      { "<Leader>sQ", "<Cmd>Telescope quickfixhistory<CR>", desc = "Quickfix history" },
      { "<Leader>uC", Util.telescope.run("colorscheme", { enable_preview = true }), desc = "Colorschemes" },
      { "<Leader>;", "<Cmd>Telescope commands<CR>", desc = "Commands" },
      { "<Leader>'", "<Cmd>Telescope resume<CR>", desc = "Resume" },
    },
    opts = {
      defaults = {
        preview = { hide_on_startup = true },
        prompt_prefix = "❯ ",
        selection_caret = " ",
        mappings = {
          i = {
            ["<C-t>"] = function(...)
              return require("trouble.providers.telescope").open_with_trouble(...)
            end,
            ["<M-i>"] = function()
              local action_state = require("telescope.actions.state")
              local line = action_state.get_current_line()
              Util.telescope.run("find_files", { no_ignore = true, default_text = line })()
            end,
            ["<M-.>"] = function()
              local action_state = require("telescope.actions.state")
              local line = action_state.get_current_line()
              Util.telescope.run("find_files", { hidden = true, default_text = line })()
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
            ["<C-j>"] = function(...)
              return require("telescope.actions").move_selection_next(...)
            end,
            ["<C-k>"] = function(...)
              return require("telescope.actions").move_selection_previous(...)
            end,
            ["<M-h>"] = function(...)
              return require("telescope.actions").results_scrolling_left(...)
            end,
            ["<M-j>"] = function(...)
              return require("telescope.actions").results_scrolling_down(...)
            end,
            ["<M-k>"] = function(...)
              return require("telescope.actions").results_scrolling_up(...)
            end,
            ["<M-l>"] = function(...)
              return require("telescope.actions").results_scrolling_right(...)
            end,
            ["<M-p>"] = function(...)
              return require("telescope.actions.layout").toggle_preview(...)
            end,
            ["<C-b>"] = { "<Left>", type = "command" },
            ["<C-f>"] = { "<Right>", type = "command" },
            ["<C-u>"] = false,
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
              ["<M-.>"] = function(...)
                require("telescope").extensions.file_browser.actions.toggle_hidden(...)
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
    end,
  },
  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    keys = {
      { "<Leader>fa", Util.telescope.run("live_grep_args", { cwd = Util.root.get }), desc = "Grep with args" },
      { "<Leader>fA", Util.telescope.run("live_grep_args", { cwd = vim.uv.cwd }), desc = "Grep with args (cwd)" },
      {
        "<Leader>fpa",
        Util.telescope.run("live_grep_args", { cwd = Util.path.get_parent_path }),
        desc = "Grep with args (parent)",
      },
      {
        "<Leader>fca",
        Util.telescope.run("live_grep_args", { cwd = vim.fn.stdpath("config") }),
        desc = "Grep with args (config)",
      },
    },
    config = function()
      require("telescope").load_extension("live_grep_args")
    end,
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    keys = {
      {
        "<Leader>fb",
        Util.telescope.run("file_browser", { cwd = Util.root.get }),
        desc = "File browser",
      },
      {
        "<Leader>fB",
        Util.telescope.run("file_browser", { cwd = vim.uv.cwd }),
        desc = "File browser (cwd)",
      },
      {
        "<Leader>fpb",
        Util.telescope.run("file_browser", { cwd = Util.path.get_parent_path }),
        desc = "File browser (parent)",
      },
    },
    config = function()
      require("telescope").load_extension("file_browser")
    end,
  },
  {
    "nvim-telescope/telescope-frecency.nvim",
    event = { "VeryLazy" },
    cmd = { "FrecencyMigrate", "FrecencyValidate" },
    keys = {
      { "<Leader>fr", "<Cmd>Telescope frecency<CR>", desc = "Frecency" },
      { "<Leader>fv", "<Cmd>FrecencyValidate<CR>", desc = "Frecency validate" },
    },
    config = function()
      require("telescope").load_extension("frecency")
    end,
  },
  {
    "debugloop/telescope-undo.nvim",
    keys = {
      { "<Leader>su", "<Cmd>Telescope undo<CR>", desc = "Undo history" },
    },
    config = function()
      require("telescope").load_extension("undo")
    end,
  },
  {
    "jvgrootveld/telescope-zoxide",
    keys = {
      { "<Leader>sz", "<Cmd>Telescope zoxide list<CR>", desc = "Zoxide" },
    },
    config = function()
      require("telescope").load_extension("zoxide")
    end,
  },

  {
    "lambdalisue/suda.vim",
    cmd = {
      "SudaRead",
      "SudaWrite",
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

  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = function()
      local keys = {
        {
          "<Leader>H",
          function()
            require("harpoon"):list():add()
          end,
          desc = "Add to harpoon",
        },
        {
          "<Leader>h",
          function()
            local harpoon = require("harpoon")
            harpoon.ui:toggle_quick_menu(harpoon:list())
          end,
          desc = "Toggle harpoon menu",
        },
      }

      for i = 1, 5 do
        table.insert(keys, {
          "<Leader>" .. i,
          function()
            require("harpoon"):list():select(i)
          end,
          desc = "Select harpoon " .. i,
        })
      end
      return keys
    end,
    opts = {},
  },

  {
    "folke/todo-comments.nvim",
    cmd = { "TodoTelescope" },
    event = "LazyFile",
    config = true,
    keys = {
      {
        "]t",
        function()
          require("todo-comments").jump_next()
        end,
        desc = "Next todo comment",
      },
      {
        "[t",
        function()
          require("todo-comments").jump_prev()
        end,
        desc = "Previous todo comment",
      },
      { "<Leader>et", "<Cmd>Trouble todo toggle<CR>", desc = "Todo (trouble)" },
      {
        "<Leader>eT",
        "<Cmd>Trouble todo toggle filter = {tag = {TODO,FIX,FIXME}}<CR>",
        desc = "Todo/Fix/Fixme (trouble)",
      },
      { "<Leader>st", "<Cmd>TodoTelescope<CR>", desc = "Todo" },
      { "<Leader>sT", "<Cmd>TodoTelescope keywords=TODO,FIX,FIXME<CR>", desc = "Todo/Fix/Fixme" },
    },
  },

  {
    "tpope/vim-rsi",
    event = {
      "InsertEnter",
      "CmdlineEnter",
    },
  },
}
