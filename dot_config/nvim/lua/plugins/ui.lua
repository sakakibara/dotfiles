local root_path = require("util.root").get
local parent_path = require("util.path").get_parent_path
return {
  {
    "kevinhwang91/nvim-ufo",
    dependencies = {
      "kevinhwang91/promise-async",
    },
    event = "LazyFile",
    keys = {
      {
        "zR",
        function()
          require("ufo").openAllFolds()
        end,
        desc = "Open all folds",
      },
      {
        "zM",
        function()
          require("ufo").closeAllFolds()
        end,
        desc = "Close all folds",
      },
      {
        "zr",
        function()
          require("ufo").openFoldsExceptKinds()
        end,
        desc = "Fold less",
      },
      {
        "zm",
        function()
          require("ufo").closeFoldsWith()
        end,
        desc = "Fold more",
      },
      {
        "zp",
        function()
          require("ufo").peekFoldedLinesUnderCursor()
        end,
        desc = "Peek fold",
      },
    },
    opts = {
      provider_selector = function(_, filetype, _)
        local ftMap = {
          markdown = { "treesitter" },
          bash = { "treesitter", "indent" },
          sh = { "treesitter", "indent" },
          zsh = { "treesitter", "indent" },
          css = { "treesitter", "indent" },
        }
        return ftMap[filetype]
      end,
      fold_virt_text_handler = function(virtText, lnum, endLnum, width, truncate)
        local newVirtText = {}
        local totalLines = vim.api.nvim_buf_line_count(0)
        local foldedLines = endLnum - lnum
        local suffix = (" 󰁂 %d %d%%"):format(foldedLines, foldedLines / totalLines * 100)
        local sufWidth = vim.fn.strdisplaywidth(suffix)
        local targetWidth = width - sufWidth
        local curWidth = 0
        for _, chunk in ipairs(virtText) do
          local chunkText = chunk[1]
          local chunkWidth = vim.fn.strdisplaywidth(chunkText)
          if targetWidth > curWidth + chunkWidth then
            table.insert(newVirtText, chunk)
          else
            chunkText = truncate(chunkText, targetWidth - curWidth)
            local hlGroup = chunk[2]
            table.insert(newVirtText, { chunkText, hlGroup })
            chunkWidth = vim.fn.strdisplaywidth(chunkText)
            if curWidth + chunkWidth < targetWidth then
              suffix = suffix .. (" "):rep(targetWidth - curWidth - chunkWidth)
            end
            break
          end
          curWidth = curWidth + chunkWidth
        end
        local rAlignAppndx = math.max(width - 5 - curWidth - sufWidth, 0)
        suffix = " ⋯  " .. (" "):rep(rAlignAppndx) .. suffix
        table.insert(newVirtText, { suffix, "MoreMsg" })
        return newVirtText
      end,
    },
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
        desc = "Delete all notifications",
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
    "j-hui/fidget.nvim",
    event = "LazyFile",
    opts = {},
  },

  {
    "mrjones2014/smart-splits.nvim",
    keys = {
      { "<M-S-h>", mode = { "n", "t" }, require("smart-splits").resize_left, desc = "Resize window left" },
      { "<M-S-j>", mode = { "n", "t" }, require("smart-splits").resize_down, desc = "Resize window down" },
      { "<M-S-k>", mode = { "n", "t" }, require("smart-splits").resize_up, desc = "Resize window up" },
      { "<M-S-l>", mode = { "n", "t" }, require("smart-splits").resize_right, desc = "Resize window right" },
      { "<M-h>", mode = { "n", "t" }, require("smart-splits").move_cursor_left, desc = "Move to left window" },
      { "<M-j>", mode = { "n", "t" }, require("smart-splits").move_cursor_down, desc = "Move to window below" },
      { "<M-k>", mode = { "n", "t" }, require("smart-splits").move_cursor_up, desc = "Move to window above" },
      { "<M-l>", mode = { "n", "t" }, require("smart-splits").move_cursor_right, desc = "Move to right window" },
      { "<leader>bh", require("smart-splits").swap_buf_left, desc = "Swap with left buffer" },
      { "<leader>bj", require("smart-splits").swap_buf_down, desc = "Swap with buffer below" },
      { "<leader>bk", require("smart-splits").swap_buf_up, desc = "Swap with buffer above" },
      { "<leader>bl", require("smart-splits").swap_buf_right, desc = "Swap with right buffer" },
    },
    opts = {
      ignored_filetypes = {
        "nofile",
        "quickfix",
        "prompt",
        "NvimTree",
      },
      default_amount = 1,
      at_edge = "stop",
      multiplexer_integration = nil,
    },
  },

  {
    "stevearc/aerial.nvim",
    event = "LazyFile",
    keys = {
      { "<leader>ua", "<cmd>AerialToggle<cr>", desc = "Toggle aerial" },
    },
    opts = function()
      local kinds_filter = require("config.kinds")
      local icons = vim.deepcopy(require("config.icons").kinds)

      local filter_kind = false
      if kinds_filter then
        ---@diagnostic disable-next-line: cast-local-type
        filter_kind = assert(vim.deepcopy(kinds_filter))
        filter_kind._ = filter_kind.default
        filter_kind.default = nil
      end

      local opts = {
        attach_mode = "global",
        backends = { "lsp", "treesitter", "markdown", "man" },
        show_guides = true,
        layout = {
          resize_to_content = false,
          win_opts = {
            winhl = "Normal:NormalFloat,FloatBorder:NormalFloat,SignColumn:SignColumnSB",
            signcolumn = "yes",
            statuscolumn = " ",
          },
        },
        icons = icons,
        filter_kind = filter_kind,
        guides = {
          mid_item = "├╴",
          last_item = "└╴",
          nested_top = "│ ",
          whitespace = "  ",
        },
        ignore = {
          filetypes = { "html" },
        },
      }
      return opts
    end,
  },

  { "MunifTanjim/nui.nvim", lazy = true },

  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    event = "LazyFile",
    opts = {
      indent = {
        char = "│",
        tab_char = "│",
      },
      exclude = {
        filetypes = {
          "Trouble",
          "csv",
          "dashboard",
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
      whitespace = { remove_blankline_trail = false },
    },
  },

  {
    "Bekaboo/deadcolumn.nvim",
    event = "UIEnter",
    init = function()
      if vim.fn.argc(-1) > 0 then
        require("deadcolumn")
      end
    end,
    opts = {},
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    event = "VeryLazy",
    cmd = {
      "TSContextEnable",
      "TSContextDisable",
      "TSContextToggle",
    },
    keys = {
      { "<leader>uc", "<cmd>TSContextToggle<cr>", desc = "Toggle treesitter context" },
    },
    opts = {
      enable = false,
      mode = "topline",
    },
  },

  {
    "stevearc/oil.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<leader>-",
        function()
          require("oil").open()
        end,
        desc = "Open parent directory",
      },
    },
    init = function()
      if vim.fn.argc(-1) == 1 then
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
          format = "%y-%m-%d %T",
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
        ["go"] = "actions.change_sort",
        ["gs"] = false,
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
        end,
      })
    end,
    dependencies = { "nvim-tree/nvim-web-devicons" },
  },

  {
    "nvim-tree/nvim-tree.lua",
    lazy = false,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    keys = {
      {
        "<leader>uf",
        function()
          require("nvim-tree.api").tree.toggle({ path = root_path() })
        end,
        desc = "Toggle nvim-tree explorer",
      },
      {
        "<leader>uF",
        function()
          require("nvim-tree.api").tree.toggle({ path = vim.loop.cwd() })
        end,
        desc = "Toggle nvim-tree explorer (cwd)",
      },
      {
        "<leader>upf",
        function()
          require("nvim-tree.api").tree.toggle({ path = parent_path() })
        end,
        desc = "Toggle nvim-tree explorer (relative)",
      },
    },
    opts = {},
  },

  {
    "antosha417/nvim-lsp-file-operations",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-tree.lua",
    },
    opts = {},
  },

  {
    "echasnovski/mini.files",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VeryLazy",
    opts = {
      windows = {
        preview = false,
        width_focus = 30,
        width_preview = 30,
      },
      options = {
        use_as_default_explorer = false,
      },
    },
    keys = {
      {
        "<leader>fm",
        function()
          require("mini.files").open(require("util.path").get_current_file_path(), true)
        end,
        desc = "Open mini files",
      },
      {
        "<leader>fM",
        function()
          require("mini.files").open(vim.loop.cwd(), true)
        end,
        desc = "Open mini files (cwd)",
      },
    },
    config = function(_, opts)
      require("mini.files").setup(opts)

      local show_dotfiles = true
      local filter_show = function(_)
        return true
      end
      local filter_hide = function(fs_entry)
        return not vim.startswith(fs_entry.name, ".")
      end

      local toggle_dotfiles = function()
        show_dotfiles = not show_dotfiles
        local new_filter = show_dotfiles and filter_show or filter_hide
        require("mini.files").refresh({ content = { filter = new_filter } })
      end

      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesBufferCreate",
        callback = function(args)
          local buf_id = args.data.buf_id
          vim.keymap.set("n", "g.", toggle_dotfiles, { buffer = buf_id, desc = "Toggle dotfiles" })
        end,
      })

      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesActionRename",
        callback = function(event)
          require("util.lsp").on_rename(event.data.from, event.data.to)
        end,
      })

      local files_set_cwd = function()
        local cur_entry_path = MiniFiles.get_fs_entry().path
        local cur_directory = vim.fs.dirname(cur_entry_path)
        vim.fn.chdir(cur_directory)
      end

      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesBufferCreate",
        callback = function(args)
          vim.keymap.set("n", "`", files_set_cwd, { buffer = args.data.buf_id, desc = "Set cwd" })
        end,
      })
    end,
  },
}
