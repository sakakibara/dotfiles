local root_path = require("util.path").get_root_path
local parent_path = require("util.path").get_parent_path
return {
  {
    "kevinhwang91/nvim-ufo",
    dependencies = {
      "kevinhwang91/promise-async",
      {
        "luukvbaal/statuscol.nvim",
        config = function()
          local builtin = require("statuscol.builtin")
          require("statuscol").setup({
            segments = {
              { text = { "%s" }, click = "v:lua.ScSa" },
              { text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
              { text = { builtin.foldfunc, " " }, click = "v:lua.ScFa" },
            }
          })
        end,
      },
    },
    event = "VeryLazy",
    keys = {
      { "zR", function() require("ufo").openAllFolds() end, desc = "Open All Folds" },
      { "zM", function() require("ufo").closeAllFolds() end, desc = "Close All Folds" },
    },
    opts = {
      provider_selector = function(_, filetype, _)
        local lspWithOutFolding = { "markdown", "bash", "sh", "bash", "zsh", "css" }
        if vim.tbl_contains(lspWithOutFolding, filetype) then
          return { "treesitter", "indent" }
        elseif filetype == "html" then
          return { "indent" }
        else
          return { "lsp", "indent" }
        end
      end,
    }
  },


  {
    "stevearc/dressing.nvim",
    lazy = true,
    init = function()
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.select = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.select(...)
      end
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.input = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.input(...)
      end
    end,
  },

  {
    "rcarriga/nvim-notify",
    keys = {
      {
        "<leader>un",
        function()
          require("notify").dismiss({ silent = true, pending = true })
        end,
        desc = "Delete all Notifications",
      },
    },
    init = function()
      vim.notify = require("notify")
    end,
    opts = {
      timeout = 3000,
      max_height = function()
        return math.floor(vim.o.lines * 0.75)
      end,
      max_width = function()
        return math.floor(vim.o.columns * 0.75)
      end,
    },
  },

  {
    "stevearc/aerial.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      attach_mode = "global",
      backends = { "lsp", "treesitter", "markdown", "man" },
      layout = { min_width = 28 },
      show_guides = true,
      filter_kind = false,
      guides = {
        mid_item = "├ ",
        last_item = "└ ",
        nested_top = "│ ",
        whitespace = "  ",
      },
    },
  },

  { "MunifTanjim/nui.nvim", lazy = true },

  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      indent = { char = "│" },
      exclude = {
        filetypes = {
          "Trouble",
          "csv",
          "help",
          "lazy",
          "mason",
          "notify",
          "oil",
          "qf",
          "text",
          "tsv",
        },
      },
      scope = { enabled = false, },
      whitespace = { remove_blankline_trail = false, },
    },
  },

  {
    "echasnovski/mini.indentscope",
    version = false,
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      symbol = "│",
      options = { try_as_border = true },
    },
    config = function(_, opts)
      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "Trouble",
          "help",
          "lazy",
          "mason",
          "notify",
        },
        callback = function()
          vim.b.miniindentscope_disable = true
        end,
      })
      require("mini.indentscope").setup(opts)
    end,
  },

  {
    "anuvyklack/hydra.nvim",
    event = "VeryLazy",
    config = function()
      local Hydra = require("hydra")
      Hydra({
        name = "Side scroll",
        mode = "n",
        body = "z",
        heads = {
          { "h", "5zh" },
          { "l", "5zl", { desc = "←/→" } },
          { "H", "zH" },
          { "L", "zL", { desc = "half screen ←/→" } },
        }
      })
    end,
  },

  {
    "stevearc/oil.nvim",
    keys = {
      { "-", function() require("oil").open() end, desc = "Open parent directory" },
    },
    init = function()
      if vim.fn.argc() == 1 then
        if require("util.path").is_dir(tostring(vim.fn.argv(0))) then
          require("oil")
        end
      end
    end,
    opts = {
      columns = {
        "icon",
        {
          "size",
          highlight = "Number",
        },
        {
          "mtime",
          highlight = "Statement",
          format = "%y-%m-%d %T"
        },
      },
      buf_options = {
        buflisted = true,
        bufhidden = "hide",
      },
      win_options = {
        wrap = false,
        signcolumn = "no",
        cursorcolumn = false,
        foldcolumn = "0",
        spell = false,
        list = false,
        conceallevel = 3,
        concealcursor = "n",
      },
      default_file_explorer = true,
      restore_win_options = true,
      skip_confirm_for_simple_edits = false,
      delete_to_trash = false,
      trash_command = "trash-put",
      prompt_save_on_select_new_entry = true,
      keymaps = {
        ["g?"] = "actions.show_help",
        ["<CR>"] = "actions.select",
        ["<C-s>"] = "actions.select_vsplit",
        ["<C-h>"] = "actions.select_split",
        ["<C-t>"] = "actions.select_tab",
        ["<C-p>"] = "actions.preview",
        ["<C-c>"] = "actions.close",
        ["<C-l>"] = "actions.refresh",
        ["-"] = "actions.parent",
        ["_"] = "actions.open_cwd",
        ["`"] = "actions.cd",
        ["~"] = "actions.tcd",
        ["g."] = "actions.toggle_hidden",
        ["gs"] = "actions.change_sort",
      },
      use_default_keymaps = true,
      view_options = {
        show_hidden = false,
        ---@diagnostic disable-next-line: unused-local
        is_hidden_file = function(name, bufnr)
          return vim.startswith(name, ".")
        end,
        ---@diagnostic disable-next-line: unused-local
        is_always_hidden = function(name, bufnr)
          return false
        end,
        sort = {
          { "type", "asc" },
          { "name", "asc" },
        },
      },
      float = {
        padding = 2,
        max_width = 0,
        max_height = 0,
        border = "rounded",
        win_options = {
          winblend = 10,
        },
        override = function(conf)
          return conf
        end,
      },
      preview = {
        max_width = 0.9,
        min_width = { 40, 0.4 },
        width = nil,
        max_height = 0.9,
        min_height = { 5, 0.1 },
        height = nil,
        border = "rounded",
        win_options = {
          winblend = 0,
        },
      },
      progress = {
        max_width = 0.9,
        min_width = { 40, 0.4 },
        width = nil,
        max_height = { 10, 0.9 },
        min_height = { 5, 0.1 },
        height = nil,
        border = "rounded",
        minimized_border = "none",
        win_options = {
          winblend = 0,
        },
      },
    },
    config = function(_, opts)
      require("oil").setup(opts)
      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("oil_disable_num", { clear = true }),
        pattern = "oil",
        callback = function()
          vim.opt_local.number = false
          vim.opt_local.relativenumber = false
        end
      })
    end,
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },

  {
    "folke/edgy.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<leader>te",
        function()
          require("edgy").toggle()
        end,
        desc = "Edgy Toggle",
      },
      { "<leader>tE", function() require("edgy").select() end, desc = "Edgy Select Window" },
    },
    opts = function()
      local opts = {
        keys = {
          ["<c-Right>"] = function(win)
            win:resize("width", 2)
          end,
          ["<c-Left>"] = function(win)
            win:resize("width", -2)
          end,
          ["<c-Up>"] = function(win)
            win:resize("height", 2)
          end,
          ["<c-Down>"] = function(win)
            win:resize("height", -2)
          end,
        },
        left = {
          {
            title = "Neo-Tree",
            ft = "neo-tree",
            filter = function(buf)
              return vim.b[buf].neo_tree_source == "filesystem"
            end,
            pinned = true,
            open = function()
              vim.api.nvim_input("<esc><space>fe")
            end,
            size = { height = 0.5 },
          },
          { title = "Neotest Summary", ft = "neotest-summary" },
          {
            title = "Neo-Tree Git",
            ft = "neo-tree",
            filter = function(buf)
              return vim.b[buf].neo_tree_source == "git_status"
            end,
            pinned = true,
            open = "Neotree position=right git_status",
          },
          {
            title = "Neo-Tree Buffers",
            ft = "neo-tree",
            filter = function(buf)
              return vim.b[buf].neo_tree_source == "buffers"
            end,
            pinned = true,
            open = "Neotree position=top buffers",
          },
          "neo-tree",
          {
            title = "Aerial",
            ft = "aerial",
            pinned = true,
            open = "AerialToggle",
          },
        },
        bottom = {
          {
            ft = "toggleterm",
            size = { height = 0.25 },
            filter = function(_, win)
              return vim.api.nvim_win_get_config(win).relative == ""
            end,
          },
          "Trouble",
          { ft = "qf", title = "QuickFix" },
          {
            ft = "help",
            size = { height = 20 },
            filter = function(buf)
              return vim.bo[buf].buftype == "help"
            end,
          },
          { ft = "spectre_panel", size = { height = 0.4 } },
          { title = "Neotest Output", ft = "neotest-output-panel", size = { height = 15 } },
        },
      }
      return opts
    end,
  },

  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    cmd = "Neotree",
    keys = {
      {
        "<leader>fe",
        function()
          require("neo-tree.command").execute({ toggle = true, dir = root_path() })
        end,
        desc = "Explorer neotree",
      },
      {
        "<leader>fE",
        function()
          require("neo-tree.command").execute({ toggle = true, dir = vim.loop.cwd() })
        end,
        desc = "Explorer neotree (cwd)",
      },
      {
        "<leader>rfe",
        function()
          require("neo-tree.command").execute({ toggle = true, dir = parent_path() })
        end,
        desc = "Explorer neotree (relative)",
      },
    },
    deactivate = function()
      vim.cmd([[Neotree close]])
    end,
    opts = {
      default_component_configs = {
        indent = {
          with_expanders = true,
          expander_collapsed = "",
          expander_expanded = "",
          expander_highlight = "NeoTreeExpander",
        },
      },
      window = {
        mappings = {
          ["<space>"] = "none",
        },
      },
      filesystem = {
        bind_to_cwd = false,
        follow_current_file = { enabled = true },
        use_libuv_file_watcher = true,
      },
      hijack_netrw_behavior = "disabled",
      sources = { "filesystem", "buffers", "git_status", "document_symbols" },
      open_files_do_not_replace_types = { "terminal", "Trouble", "qf", "Outline", "edgy" },
    },
  },
}
