return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts_extend = { "spec" },
    opts = {
      defaults = {},
      icons = { rules = false },
      spec = {
        {
          mode = { "n", "v" },
          { "<Leader><Tab>", group = "tabs" },
          {
            "<Leader>b",
            group = "buffer",
            expand = function()
              return require("which-key.extras").expand.buf()
            end,
          },
          { "<Leader>c", group = "code" },
          { "<Leader>d", group = "debug" },
          { "<Leader>da", group = "adapters" },
          { "<Leader>e", group = "editor" },
          { "<Leader>f", group = "file" },
          { "<Leader>fc", group = "config" },
          { "<Leader>fp", group = "parent" },
          { "<Leader>g", group = "git" },
          { "<Leader>q", group = "quit" },
          { "<Leader>r", group = "rest" },
          { "<Leader>s", group = "search" },
          { "<Leader>t", group = "test" },
          { "<Leader>u", group = "ui" },
          {
            "<Leader>w",
            group = "windows",
            proxy = "<C-w>",
            expand = function()
              return require("which-key.extras").expand.win()
            end,
          },
          { "<Leader>ut", group = "terminal" },
          { "[", group = "prev" },
          { "[o", group = "enable option" },
          { "]", group = "next" },
          { "]o", group = "disable option" },
          { "gs", group = "surround" },
          { "yo", group = "toggle" },
        },
      },
    },
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
    event = {
      "InsertEnter",
      "CmdlineEnter",
    },
    opts = {
      default_mappings = false,
      mappings = {
        i = {
          j = {
            k = "<Esc>",
          },
        },
        c = {
          j = {
            k = "<Esc>",
          },
        },
      },
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
    "lewis6991/gitsigns.nvim",
    event = "LazyFile",
    opts = {
      signs = {
        add = { text = "┃" },
        change = { text = "┃" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "┃" },
        untracked = { text = "┃" },
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
    "MagicDuck/grug-far.nvim",
    opts = { headerMaxWidth = 80 },
    cmd = "GrugFar",
    keys = {
      {
        "<Leader>sr",
        function()
          local grug = require("grug-far")
          local ext = vim.bo.buftype == "" and vim.fn.expand("%:e")
          grug.open({
            transient = true,
            prefills = {
              filesFilter = ext and ext ~= "" and "*." .. ext or nil,
            },
          })
        end,
        mode = { "n", "v" },
        desc = "Search and replace",
      },
    },
  },

  {
    "stevearc/quicker.nvim",
    event = "VeryLazy",
    keys = {
      {
        "<Leader>el",
        function()
          require("quicker").toggle({ loclist = true })
        end,
        desc = "Toggle location list",
      },
      {
        "<Leader>eq",
        function()
          require("quicker").toggle()
        end,
        desc = "Toggle quickfix list",
      },
    },
    opts = {},
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
    },
    opts = {},
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

  {
    "idanarye/nvim-impairative",
    event = { "VeryLazy" },
    opts = function()
      local conceallevel = vim.o.conceallevel > 0 and vim.o.conceallevel or 3

      local function diagnostic_goto(next, severity)
        local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
        severity = severity and vim.diagnostic.severity[severity] or nil
        return function()
          go({ severity = severity })
        end
      end

      return {
        enable = "[o",
        disable = "]o",
        toggle = "yo",
        toggling = function(h)
          h:option({
            key = "o",
            option = "conceallevel",
            values = { [true] = conceallevel, [false] = 0 },
          })
          h:option({
            key = "b",
            option = "background",
            values = { [true] = "light", [false] = "dark" },
          })
          h:option({
            key = "c",
            option = "cursorline",
          })
          h:getter_setter({
            key = "d",
            name = "diff mode",
            get = function()
              return vim.o.diff
            end,
            set = function(value)
              if value then
                vim.cmd.diffthis()
              else
                vim.cmd.diffoff()
              end
            end,
          })
          h:getter_setter({
            key = "D",
            name = "diagnostics",
            get = function()
              return vim.diagnostic.is_enabled({ bufnr = 0 })
            end,
            set = function(value)
              if value then
                vim.diagnostic.enable()
              else
                vim.diagnostic.enable(false)
              end
            end,
          })
          h:getter_setter({
            key = "f",
            name = "auto format (global)",
            get = function()
              return Util.format.enabled()
            end,
            set = function(value)
              vim.g.autoformat = value
              vim.b.autoformat = nil
              Util.format.info()
            end,
          })
          h:getter_setter({
            key = "F",
            name = "auto format (buffer)",
            get = function()
              return Util.format.enabled()
            end,
            set = function(value)
              vim.b.autoformat = value
              Util.format.info()
            end,
          })
          h:option({
            key = "h",
            option = "hlsearch",
          })
          h:option({
            key = "i",
            option = "ignorecase",
          })
          h:getter_setter({
            key = "I",
            name = "inlay hints",
            get = function()
              return vim.lsp.inlay_hint.is_enabled({ bufnr = 0 })
            end,
            set = function(value)
              if value then
                vim.lsp.inlay_hint.enable()
              else
                vim.lsp.inlay_hint.enable(false)
              end
            end,
          })
          h:option({
            key = "l",
            option = "list",
          })
          h:option({
            key = "n",
            option = "number",
          })
          h:option({
            key = "r",
            option = "relativenumber",
          })
          h:option({
            key = "s",
            option = "spell",
          })
          h:option({
            key = "t",
            option = "colorcolumn",
            values = { [true] = "+1", [false] = "" },
          })
          h:getter_setter({
            key = "T",
            name = "treesitter highlight",
            get = function()
              return vim.b.ts_highlight
            end,
            set = function(value)
              if value then
                vim.treesitter.start()
              else
                vim.treesitter.stop()
              end
            end,
          })
          h:option({
            key = "u",
            option = "cursorcolumn",
          })
          h:option({
            key = "v",
            option = "virtualedit",
            values = { [true] = "all", [false] = "" },
          })
          h:option({
            key = "w",
            option = "wrap",
          })
          h:getter_setter({
            key = "x",
            name = "Vim's 'cursorline' and 'cursorcolumn' options both",
            get = function()
              return vim.o.cursorline and vim.o.cursorcolumn
            end,
            set = function(value)
              vim.o.cursorline = value
              vim.o.cursorcolumn = value
            end,
          })
        end,

        backward = "[",
        forward = "]",
        operations = function(h)
          h:command_pair({
            key = "a",
            backward = "previous",
            forward = "next",
          })
          h:command_pair({
            key = "A",
            backward = "first",
            forward = "last",
          })
          h:command_pair({
            key = "b",
            backward = "bprevious",
            forward = "bnext",
          })
          h:command_pair({
            key = "B",
            backward = "bfirst",
            forward = "blast",
          })
          h:command_pair({
            key = "l",
            backward = "lprevious",
            forward = "lnext",
          })
          h:command_pair({
            key = "L",
            backward = "lfirst",
            forward = "llast",
          })
          h:command_pair({
            key = "<C-l>",
            backward = "lpfile",
            forward = "lnfile",
          })
          h:unified_function({
            key = "q",
            desc = "{Previous|Next} trouble/quickfix item",
            fun = function(direction)
              if Util.plugin.has("trouble.nvim") and require("trouble").is_open() then
                if direction == "backward" then
                  ---@diagnostic disable-next-line: missing-parameter, missing-fields
                  require("trouble").prev({ skip_groups = true, jump = true })
                else
                  ---@diagnostic disable-next-line: missing-parameter, missing-fields
                  require("trouble").next({ skip_groups = true, jump = true })
                end
              elseif Util.plugin.has("qf_helper.nvim") then
                if direction == "backward" then
                  vim.cmd("QPrev")
                else
                  vim.cmd("QNext")
                end
              else
                local ok, err
                if direction == "backward" then
                  ok, err = pcall(vim.cmd.cprev)
                else
                  ok, err = pcall(vim.cmd.cnext)
                end
                if not ok then
                  vim.notify(err, vim.log.levels.ERROR)
                end
              end
            end,
          })
          h:command_pair({
            key = "Q",
            backward = "cfirst",
            forward = "clast",
          })
          h:command_pair({
            key = "<C-q>",
            backward = "cpfile",
            forward = "cnfile",
          })
          h:command_pair({
            key = "t",
            backward = "tprevious",
            forward = "tnext",
          })
          h:command_pair({
            key = "T",
            backward = "tfirst",
            forward = "tlast",
          })
          h:command_pair({
            key = "<C-t>",
            backward = "ptprevious",
            forward = "ptnext",
          })
          h:unified_function({
            key = "f",
            desc = "Jump to the {previous|next} file in the directory tree",
            fun = function(direction)
              local win_info = vim.fn.getwininfo(vim.api.nvim_get_current_win())[1] or {}
              if win_info.quickfix == 1 then
                local cmd
                if win_info.loclist == 1 then
                  if direction == "backward" then
                    cmd = "lolder"
                  else
                    cmd = "lnewer"
                  end
                else
                  if direction == "backward" then
                    cmd = "colder"
                  else
                    cmd = "cnewer"
                  end
                end
                ---@diagnostic disable-next-line: param-type-mismatch
                local ok, err = pcall(vim.cmd, {
                  cmd = cmd,
                  count = vim.v.count1,
                })
                if not ok then
                  vim.api.nvim_err_writeln(err)
                end
              else
                local it = require("impairative.helpers").walk_files_tree(vim.fn.expand("%"), direction == "backward")
                local path
                path = it:nth(vim.v.count1)
                if path then
                  require("impairative.util").jump_to({ filename = path })
                end
              end
            end,
          })
          h:jump_in_buf({
            key = "n",
            desc = "Jump to the {previous|next} SCM conflict marker or diff/path hunk",
            extreme = {
              key = "N",
              desc = "Jump to the {first|last} SCM conflict marker or diff/path hunk",
            },
            fun = require("impairative.helpers").conflict_marker_locations,
          })
          h:unified_function({
            key = "<Space>",
            desc = "Add blank line(s) {above|below} the current line",
            fun = function(direction)
              local line_number = vim.api.nvim_win_get_cursor(0)[1]
              if direction == "backward" then
                line_number = line_number - 1
              end
              local lines = vim.fn["repeat"]({ "" }, vim.v.count1)
              vim.api.nvim_buf_set_lines(0, line_number, line_number, true, lines)
            end,
          })
          h:range_manipulation({
            key = "e",
            line_key = true,
            desc = "Exchange the line(s) with [count] lines {above|below} it",
            fun = function(args)
              local target
              if args.direction == "backward" then
                target = args.start_line - args.count1 - 1
              else
                target = args.end_line + args.count1
              end
              vim.cmd({
                cmd = "move",
                range = { args.start_line, args.end_line },
                args = { target },
              })
            end,
          })
          h:text_manipulation({
            key = "u",
            line_key = true,
            desc = "{Encode|Decode} URL",
            backward = require("impairative.helpers").encode_url,
            forward = require("impairative.helpers").decode_url,
          })
          h:text_manipulation({
            key = "x",
            line_key = true,
            desc = "{Encode|Decode} XML",
            backward = require("impairative.helpers").encode_xml,
            forward = require("impairative.helpers").decode_xml,
          })
          h:text_manipulation({
            key = "y",
            line_key = true,
            desc = "{Escape|Unescape} strings (C escape rules)",
            backward = require("impairative.helpers").encode_string,
            forward = require("impairative.helpers").decode_string,
          })
          h:text_manipulation({
            key = "C",
            line_key = true,
            desc = "{Escape|Unescape} strings (C escape rules)",
            backward = require("impairative.helpers").encode_string,
            forward = require("impairative.helpers").decode_string,
          })
          h:function_pair({
            key = "d",
            desc = "{Previous|Next} diagnostic",
            backward = diagnostic_goto(false),
            forward = diagnostic_goto(true),
          })
          h:function_pair({
            key = "r",
            desc = "{Previous|Next} error",
            backward = diagnostic_goto(false, "ERROR"),
            forward = diagnostic_goto(true, "ERROR"),
          })
          h:function_pair({
            key = "w",
            desc = "{Previous|Next} warning",
            backward = diagnostic_goto(false, "WARN"),
            forward = diagnostic_goto(true, "WARN"),
          })
        end,
      }
    end,
  },

  {
    import = "plugins.extras.editor.fzf",
    enabled = function()
      return Util.pick.want() == "fzf"
    end,
  },

  {
    import = "plugins.extras.editor.telescope",
    enabled = function()
      return Util.pick.want() == "telescope"
    end,
  },
}
