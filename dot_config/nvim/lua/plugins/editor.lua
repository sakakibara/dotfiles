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
            require("trouble").prev({ skip_groups = true, jump = true })
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
    "alker0/chezmoi.vim",
    init = function()
      vim.g["chezmoi#use_tmp_buffer"] = 1
      vim.g["chezmoi#source_dir_path"] = os.getenv("HOME") .. "/.local/share/chezmoi"
    end,
  },

  {
    "xvzc/chezmoi.nvim",
    keys = {
      {
        "<leader>sc",
        function()
          require("telescope").extensions.chezmoi.find_files()
        end,
        desc = "Chezmoi",
      },
    },
    opts = {
      edit = {
        watch = false,
        force = false,
      },
      notification = {
        on_open = true,
        on_apply = true,
        on_watch = false,
      },
      telescope = {
        select = { "<CR>" },
      },
    },
    init = function()
      vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        pattern = { os.getenv("HOME") .. "/.local/share/chezmoi/*" },
        callback = function()
          vim.schedule(require("chezmoi.commands.__edit").watch)
        end,
      })
    end,
  },

  {
    "tpope/vim-rsi",
    event = {
      "InsertEnter",
      "CmdlineEnter",
    },
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
