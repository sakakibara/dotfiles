local util_telescope = require("util.telescope")
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
        ["<leader>s"] = { name = "+search" },
        ["<leader>u"] = { name = "+ui" },
        ["["] = { name = "+prev" },
        ["]"] = { name = "+next" },
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.register(opts.defaults)
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
      { "<leader>bd", function() require("mini.bufremove").delete(0, false) end, desc = "Delete Buffer" },
      { "<leader>bD", function() require("mini.bufremove").delete(0, true) end, desc = "Delete Buffer (Force)" },
    },
  },

  {
    "max397574/better-escape.nvim",
    event = "InsertEnter",
    opts = {
      mapping = {"jk"}
    },
  },

  {
    "monaqa/dial.nvim",
    keys = {
      {
        "<C-a>",
        function() return require("dial.map").inc_normal() end,
        expr = true,
        desc = "Increment",
      },
      {
        "<C-x>",
        function() return require("dial.map").dec_normal() end,
        expr = true,
        desc = "Decrement",
      },
      {
        "<C-a>",
        function() return require("dial.map").inc_visual() end,
        expr = true,
        desc = "Increment",
        mode = "v",
      },
      {
        "<C-x>",
        function() return require("dial.map").dec_visual() end,
        expr = true,
        desc = "Decrement",
        mode = "v",
      },
      {
        "g<C-a>",
        function() return require("dial.map").inc_gvisual() end,
        expr = true,
        desc = "Increment",
        mode = "v",
      },
      {
        "g<C-x>",
        function() return require("dial.map").dec_gvisual() end,
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
    "RRethy/vim-illuminate",
    event = "BufReadPost",
    opts = { delay = 200 },
    config = function(_, opts)
      require("illuminate").configure(opts)
    end,
    keys = {
      { "]]", function() require("illuminate").goto_next_reference(false) end, desc = "Next Reference", },
      { "[[", function() require("illuminate").goto_prev_reference(false) end, desc = "Prev Reference" },
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
      { "s", mode = { "n", "x" }, function() require("flash").jump() end, desc = "Flash" },
      { "S", mode = { "n" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
      { "z", mode = { "o" }, function() require("flash").jump() end, desc = "Flash" },
      { "Z", mode = { "o", "x" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
      { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
      { "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
      { "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
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
    event = "BufReadPre",
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
        map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end, "Blame Line")
        map("n", "<leader>ghd", gs.diffthis, "Diff This")
        map("n", "<leader>ghD", function() gs.diffthis("~") end, "Diff This ~")
        map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
      end,
    },
  },

  {
    "nvim-pack/nvim-spectre",
    cmd = "Spectre",
    opts = { open_cmd = "noswapfile vnew" },
    keys = {
      { "<leader>sr", function() require("spectre").open() end, desc = "Spectre" },
    },
  },

  {
    "kevinhwang91/nvim-bqf",
    dependencies = {
      "lukas-reineke/indent-blankline.nvim",
    },
    event = "VeryLazy",
    opts = {},
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
      { "<leader><space>", util_telescope.func("files", { cwd = util_telescope.root }), desc = "Find Files (root dir)" },
      { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>ff", util_telescope.func("find_files", { cwd = util_telescope.basedir }), desc = "Find Files" },
      { "<leader>pff", util_telescope.func("files", { cwd = util_telescope.root }), desc = "Find Files (root dir)" },
      { "<leader>pfF", util_telescope.func("files", { cwd = false }), desc = "Find Files (cwd)" },
      { '<leader>s"', "<cmd>Telescope registers<cr>", desc = "Registers" },
      { "<leader>ss", "<cmd>Telescope current_buffer_fuzzy_find<cr>", desc = "Buffer" },
      { "<leader>sc", "<cmd>Telescope command_history<cr>", desc = "Command History" },
      { "<leader>sg", util_telescope.func("live_grep", { cwd = util_telescope.basedir }), desc = "Grep" },
      { "<leader>psg", util_telescope.func("live_grep", { cwd = util_telescope.root }), desc = "Grep (root dir)" },
      { "<leader>psG", util_telescope.func("live_grep", { cwd = false }), desc = "Grep (cwd)" },
      { "<leader>sw", util_telescope.func("grep_string", { cwd = util_telescope.basedir, word_match = "-w" }), desc = "Word" },
      { "<leader>psw", util_telescope.func("grep_string", { cwd = util_telescope.root, word_match = "-w" }), desc = "Word (root dir)" },
      { "<leader>psW", util_telescope.func("grep_string", { cwd = false, word_match = "-w" }), desc = "Word (cwd)" },
      { "<leader>sw", util_telescope.func("grep_string", { cwd = util_telescope.basedir }), mode = "v", desc = "Selection" },
      { "<leader>psw", util_telescope.func("grep_string", { cwd = util_telescope.root }), mode = "v", desc = "Selection (root dir)" },
      { "<leader>psW", util_telescope.func("grep_string", { cwd = false }), mode = "v", desc = "Selection (cwd)" },
      { "<leader>ht", "<cmd>Telescope help_tags<cr>", desc = "Help Tags" },
      { "<leader>hk", "<cmd>Telescope keymaps<cr>", desc = "Keymaps" },
      { "<leader>tt", util_telescope.func("colorscheme", { enable_preview = true }), desc = "Colorschemes" },
      { "<leader>:", "<cmd>Telescope commands<cr>", desc = "Commands" },
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
              util_telescope.func("find_files", { no_ignore = true, default_text = line })()
            end,
            ["<a-h>"] = function()
              local action_state = require("telescope.actions.state")
              local line = action_state.get_current_line()
              util_telescope.func("find_files", { hidden = true, default_text = line })()
            end,
            ["<C-Down>"] = function(...)
              return require("telescope.actions").cycle_history_next(...)
            end,
            ["<C-Up>"] = function(...)
              return require("telescope.actions").cycle_history_prev(...)
            end,
            ["<C-j>"] = function(...)
              return require("telescope.actions").move_selection_next(...)
            end,
            ["<C-k>"] = function(...)
              return require("telescope.actions").move_selection_previous(...)
            end,
            ["<C-n>"] = function(...)
              return require("telescope.actions").preview_scrolling_down(...)
            end,
            ["<C-p>"] = function(...)
              return require("telescope.actions").preview_scrolling_up(...)
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
    end
  },
  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    keys = {
      { "<leader>/", "<cmd>Telescope live_grep_args<cr>", desc = "Live Grep Args" },
      { "<leader>p/", util_telescope.func("live_grep_args"), desc = "Live Grep Args (cwd)" },
    },
    config = function()
      require("telescope").load_extension("live_grep_args")
    end
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    keys = {
      {
        "<leader>fb",
        util_telescope.func("file_browser", { cwd = util_telescope.basedir }),
        desc = "Find browser",
      },
    },
    config = function()
      require("telescope").load_extension("file_browser")
    end
  },
  {
    "nvim-telescope/telescope-frecency.nvim",
    event = "VeryLazy",
    keys = {
      { "<leader>fr", "<cmd>Telescope frecency<cr>", desc = "Recent" },
    },
    config = function()
      require("telescope").load_extension("frecency")
    end
  },
  {
    "debugloop/telescope-undo.nvim",
    keys = {
      { "<leader>su", "<cmd>Telescope undo<cr>", desc = "Undo history" },
    },
    config = function()
      require("telescope").load_extension("undo")
    end
  },
  {
    "jvgrootveld/telescope-zoxide",
    keys = {
      { "<leader>sz", "<cmd>Telescope zoxide list<cr>", desc = "Zoxide" },
    },
    config = function()
      require("telescope").load_extension("zoxide")
    end
  },

  {
    "mickael-menu/zk-nvim",
    keys = {
      { "<leader>nn", "<cmd>ZkNew { title = vim.fn.input('Title: ') }<cr>", desc = "Create zk note" },
      { "<leader>nf", "<cmd>ZkNotes { sort = { 'modified' } }<cr>", desc = "Open zk note" },
      { "<leader>ns", "<cmd>ZkGrep<cr>", desc = "Search zk note" },
      { "<leader>nt", "<cmd>ZkTags<cr>", desc = "Open zk note by tags" },
      { "<leader>no", "<cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search : ') } }<cr>", desc = "Open zk note by tags" },
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
        }
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
