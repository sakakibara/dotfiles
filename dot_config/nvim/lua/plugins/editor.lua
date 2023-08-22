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
    "echasnovski/mini.basics",
    config = function(_, opts)
      require("mini.basics").setup(opts)
      vim.keymap.del("n", "<C-z>")
      vim.keymap.del("i", "<C-z>")
    end
  },

  {
    "echasnovski/mini.bufremove",
    keys = {
      { "<leader>bd", function() require("mini.bufremove").delete(0, false) end, desc = "Delete Buffer" },
      { "<leader>bD", function() require("mini.bufremove").delete(0, true) end, desc = "Delete Buffer (Force)" },
    },
  },

  {
    "chrisgrieser/nvim-genghis",
    dependencies = "stevearc/dressing.nvim",
    keys = {
      { "<leader>fy", function() require("genghis").copyFilename() end, desc = "Copy File Name" },
      { "<leader>fY", function() require("genghis").copyFilepath() end, desc = "Copy File Path" },
      { "<leader>fx", function() require("genghis").chmodx() end, desc = "File Chmod" },
      { "<leader>fm", function() require("genghis").renameFile() end, desc = "Rename File" },
      { "<leader>fM", function() require("genghis").moveAndRenameFile() end, desc = "Move and Rename File" },
      { "<leader>fn", function() require("genghis").createNewFile() end, desc = "Create New File" },
      { "<leader>fd", function() require("genghis").duplicateFile() end, desc = "Create Duplicate File" },
      { "<leader>fD", function() require("genghis").trashFile() end, desc = "Trash File" },
    }
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
    "mickael-menu/zk-nvim",
    keys = {
      { "<leader>nn", "<cmd>ZkNew { title = vim.fn.input('Title: ') }<cr>", desc = "Create zk note" },
      { "<leader>nf", "<cmd>ZkNotes { sort = { 'modified' } }<cr>", desc = "Open zk note" },
      { "<leader>ns", "<cmd>ZkGrep<cr>", desc = "Search zk note" },
      { "<leader>nt", "<cmd>ZkTags<cr>", desc = "Open zk note by tags" },
      { "<leader>no", "<cmd>ZkNotes { sort = { 'modified' }, match = { vim.fn.input('Search : ') } }<cr>", desc = "Open zk note by tags" },
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
}
