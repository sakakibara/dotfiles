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
    "folke/noice.nvim",
    event = "VeryLazy",
    opts = {
      lsp = {
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"] = true,
          ["cmp.entry.get_documentation"] = true,
        },
        hover = {
          silent = true,
        },
      },
      routes = {
        {
          filter = {
            event = "msg_show",
            any = {
              { find = "%d+L, %d+B" },
              { find = "; after #%d+" },
              { find = "; before #%d+" },
            },
          },
          view = "mini",
        },
      },
      presets = {
        command_palette = true,
        long_message_to_split = true,
      },
    },
    keys = {
      { "<Leader>sn", "", desc = "Noice" },
      {
        "<S-Enter>",
        function()
          require("noice").redirect(vim.fn.getcmdline())
        end,
        mode = "c",
        desc = "Redirect cmdline",
      },
      {
        "<Leader>snl",
        function()
          require("noice").cmd("last")
        end,
        desc = "Noice last message",
      },
      {
        "<Leader>snh",
        function()
          require("noice").cmd("history")
        end,
        desc = "Noice history",
      },
      {
        "<Leader>sna",
        function()
          require("noice").cmd("all")
        end,
        desc = "Noice all",
      },
      {
        "<Leader>snd",
        function()
          require("noice").cmd("dismiss")
        end,
        desc = "Dismiss all",
      },
      {
        "<Leader>snt",
        function()
          require("noice").cmd("pick")
        end,
        desc = "Noice picker (Telescope/FzfLua)",
      },
      {
        "<C-f>",
        function()
          if not require("noice.lsp").scroll(4) then
            return "<c-f>"
          end
        end,
        silent = true,
        expr = true,
        desc = "Scroll forward",
        mode = { "i", "n", "s" },
      },
      {
        "<C-b>",
        function()
          if not require("noice.lsp").scroll(-4) then
            return "<c-b>"
          end
        end,
        silent = true,
        expr = true,
        desc = "Scroll backward",
        mode = { "i", "n", "s" },
      },
    },
    config = function(_, opts)
      if vim.o.filetype == "lazy" then
        vim.cmd([[messages clear]])
      end
      require("noice").setup(opts)
    end,
  },

  {
    "j-hui/fidget.nvim",
    enabled = function()
      return not Util.plugin.has("noice.nvim")
    end,
    event = "LazyFile",
    opts = {},
  },

  {
    "stevearc/aerial.nvim",
    enabled = false,
    event = "LazyFile",
    keys = {
      { "<Leader>ua", "<Cmd>AerialToggle<CR>", desc = "Toggle aerial" },
    },
    opts = function()
      local icons = vim.deepcopy(Util.config.icons.kinds)

      local filter_kind = false
      if Util.config.kind_filter then
        filter_kind = assert(vim.deepcopy(Util.config.kind_filter))
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
      { "<Leader>uc", "<Cmd>TSContextToggle<CR>", desc = "Toggle treesitter context" },
    },
    opts = {
      enable = false,
      mode = "topline",
    },
  },

  {
    "stevearc/oil.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VeryLazy",
    keys = {
      {
        "<Leader>-",
        function()
          require("oil").open()
        end,
        desc = "Open parent directory",
      },
    },
    init = function()
      if vim.fn.argc(-1) == 1 then
        if Util.path.is_dir(tostring(vim.fn.argv(0))) then
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
      mappings = {
        toggle_hidden = "g.",
        change_cwd = "gc",
        go_in_horizontal = "<C-w>s",
        go_in_vertical = "<C-w>v",
        go_in_horizontal_plus = "<C-w>S",
        go_in_vertical_plus = "<C-w>V",
      },
      windows = {
        preview = true,
        width_focus = 30,
        width_preview = 30,
      },
      options = {
        use_as_default_explorer = false,
      },
    },
    keys = {
      {
        "<Leader>fm",
        function()
          require("mini.files").open(Util.path.get_current_file_path(), true)
        end,
        desc = "Open mini files",
      },
      {
        "<Leader>fM",
        function()
          require("mini.files").open(vim.uv.cwd(), true)
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

      local map_split = function(buf_id, lhs, direction, close_on_file)
        local rhs = function()
          local new_target_window
          local cur_target_window = require("mini.files").get_target_window()
          if cur_target_window ~= nil then
            vim.api.nvim_win_call(cur_target_window, function()
              vim.cmd("belowright " .. direction .. " split")
              new_target_window = vim.api.nvim_get_current_win()
            end)

            require("mini.files").set_target_window(new_target_window)
            require("mini.files").go_in({ close_on_file = close_on_file })
          end
        end

        local desc = "Open in " .. direction .. " split"
        if close_on_file then
          desc = desc .. " and close"
        end
        vim.keymap.set("n", lhs, rhs, { buffer = buf_id, desc = desc })
      end

      local files_set_cwd = function()
        local cur_entry_path = MiniFiles.get_fs_entry().path
        local cur_directory = vim.fs.dirname(cur_entry_path)
        if cur_directory ~= nil then
          vim.fn.chdir(cur_directory)
        end
      end

      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesBufferCreate",
        callback = function(args)
          local buf_id = args.data.buf_id

          vim.keymap.set(
            "n",
            opts.mappings.toggle_hidden,
            toggle_dotfiles,
            { buffer = buf_id, desc = "Toggle hidden files" }
          )

          vim.keymap.set("n", opts.mappings.change_cwd, files_set_cwd, { buffer = args.data.buf_id, desc = "Set cwd" })

          map_split(buf_id, opts.mappings.go_in_horizontal, "horizontal", false)
          map_split(buf_id, opts.mappings.go_in_vertical, "vertical", false)
          map_split(buf_id, opts.mappings.go_in_horizontal_plus, "horizontal", true)
          map_split(buf_id, opts.mappings.go_in_vertical_plus, "vertical", true)
        end,
      })

      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesActionRename",
        callback = function(event)
          Snacks.rename.on_rename_file(event.data.from, event.data.to)
        end,
      })
    end,
  },

  {
    "b0o/incline.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    event = "VeryLazy",
    keys = {
      {
        "<Leader>ui",
        function()
          require("incline").toggle()
        end,
        desc = "Toggle incline",
      },
    },
    opts = {
      window = {
        padding = 0,
        margin = { horizontal = 0 },
      },
      ignore = {
        buftypes = { "help", "nofile", "nowrite", "quickfix", "terminal", "prompt" },
      },
      hide = {
        cursorline = "focused_win",
      },
      render = function(props)
        local devicons = require("nvim-web-devicons")

        local function shorten_path_styled(path, opts)
          opts = opts or {}
          local head_style = opts.head_style or {}
          local tail_style = opts.tail_style or {}
          local _, head, tail = Util.path.format_path(
            path,
            vim.tbl_extend("force", opts, {
              return_segments = true,
              last_separator = true,
            })
          )
          return {
            head and vim.list_extend(head_style, { head }) or "",
            vim.list_extend(tail_style, { tail }),
          }
        end

        local colors = {
          fg = Util.ui.color("Normal"),
          fg_nc = Util.ui.dim(Util.ui.color("Normal"), 0.75),
          bg = Util.ui.color("Normal", true),
          red = Util.ui.color("Error"),
        }

        local function get_icon(buf)
          local bufname = Util.path.buf_get_name(buf)
          local extension = vim.fn.fnamemodify(bufname, ":e")
          local icon, icon_color
          icon, icon_color = devicons.get_icon_color(bufname, extension, { default = true })
          return {
            icon = icon,
            fg = icon_color,
          }
        end

        local function get_file_path(buf, focused, fg, fg_nc)
          local bufname = Util.path.buf_get_name(buf)
          local fname = shorten_path_styled(bufname, {
            short_len = 3,
            tail_count = 2,
            max_segments = 3,
            replace_home = true,
            ellipsis = true,
            no_name = true,
            head_style = { guifg = fg_nc },
            tail_style = { guifg = focused and fg or fg_nc },
          })
          return fname
        end

        local file_path = get_file_path(props.buf, props.focused, colors.fg, colors.fg_nc)
        local icon = get_icon(props.buf)
        local modified = vim.bo[props.buf].modified

        return {
          {
            {
              " ",
            },
            {
              icon.icon,
              " ",
              guifg = props.focused and icon.fg or colors.fg_nc,
            },
            {
              " ",
            },
            {
              file_path,
              gui = modified and "bold,italic" or nil,
            },
            { modified and " [+]" or "", guifg = colors.red },
            {
              " ",
            },
            guibg = colors.bg,
          },
        }
      end,
    },
  },

  {
    "snacks.nvim",
    opts = {
      indent = { enabled = true },
      input = { enabled = true },
      notifier = { enabled = true },
      scope = { enabled = true },
      scroll = { enabled = true },
      statuscolumn = { enabled = false },
      words = { enabled = true },
    },
    keys = {
      {
        "<Leader>n",
        function()
          Snacks.notifier.show_history()
        end,
        desc = "Notification history",
      },
      {
        "<Leader>un",
        function()
          Snacks.notifier.hide()
        end,
        desc = "Dismiss all notifications",
      },
    },
  },
}
