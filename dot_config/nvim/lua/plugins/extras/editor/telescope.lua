local have_make = vim.fn.executable("make") == 1
local have_cmake = vim.fn.executable("cmake") == 1

local picker = {
  name = "telescope",
  commands = {
    files = "find_files",
  },
  open = function(command, opts)
    opts = opts or {}
    opts.follow = opts.follow ~= false
    if opts.cwd and opts.cwd ~= vim.uv.cwd() then
      local function open_cwd_dir()
        local action_state = require("telescope.actions.state")
        local line = action_state.get_current_line()
        Util.pick.open(
          command,
          vim.tbl_deep_extend("force", {}, opts or {}, {
            root = false,
            default_text = line,
          })
        )
      end
      opts.attach_mappings = function(_, map)
        map("i", "<M-c>", open_cwd_dir, { desc = "Open cwd directory" })
        return true
      end
    end

    local builtin = require("telescope.builtin")[command]
    if builtin then
      builtin(opts)
    else
      require("telescope").extensions[command][command](opts)
    end
  end,
}
if not Util.pick.register(picker) then
  return {}
end

return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = "Telescope",
    enabled = function()
      return Util.pick.want() == "telescope"
    end,
    version = false,
    dependencies = {
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = have_make and "make"
          or "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
        enabled = have_make or have_cmake,
        config = function(plugin)
          Util.plugin.on_load("telescope.nvim", function()
            local ok, err = pcall(require("telescope").load_extension, "fzf")
            if not ok then
              local lib = plugin.dir .. "/build/libfzf." .. (Util.is_win() and "dll" or "so")
              if not vim.uv.fs_stat(lib) then
                Util.warn("`telescope-fzf-native.nvim` not built. Rebuilding...")
                require("lazy").build({ plugins = { plugin }, show = false }):wait(function()
                  Util.info("Rebuilding `telescope-fzf-native.nvim` done.\nPlease restart Neovim.")
                end)
              else
                Util.error("Failed to load `telescope-fzf-native.nvim`:\n" .. err)
              end
            end
          end)
        end,
      },
    },
    keys = {
      {
        "<Leader>,",
        "<Cmd>Telescope buffers sort_mru=true sort_lastused=true<CR>",
        desc = "Switch buffer",
      },
      { "<Leader>/", Util.pick("live_grep"), desc = "Grep (root)" },
      { "<Leader>:", "<Cmd>Telescope command_history<CR>", desc = "Command history" },
      { "<Leader><Space>", Util.pick("files"), desc = "Find files (root)" },
      { "<Leader>fb", "<Cmd>Telescope buffers sort_mru=true sort_lastused=true<CR>", desc = "Buffers" },
      { "<Leader>fc", Util.pick.config_files(), desc = "Find config file" },
      { "<Leader>ff", Util.pick("files"), desc = "Find files (root)" },
      { "<Leader>fF", Util.pick("files", { root = false }), desc = "Find files (cwd)" },
      { "<Leader>fg", "<Cmd>Telescope git_files<CR>", desc = "Find files (git files)" },
      { "<Leader>fr", "<Cmd>Telescope oldfiles<CR>", desc = "Recent" },
      { "<Leader>fR", Util.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
      { "<Leader>gc", "<Cmd>Telescope git_commits<CR>", desc = "Commits" },
      { "<Leader>gs", "<Cmd>Telescope git_status<CR>", desc = "Status" },
      { '<Leader>s"', "<Cmd>Telescope registers<CR>", desc = "Registers" },
      { "<Leader>sb", "<Cmd>Telescope current_buffer_fuzzy_find<CR>", desc = "Buffer" },
      { "<Leader>sc", "<Cmd>Telescope command_history<CR>", desc = "Command history" },
      { "<Leader>sC", "<Cmd>Telescope commands<CR>", desc = "Commands" },
      { "<Leader>sd", "<Cmd>Telescope diagnostics bufnr=0<CR>", desc = "Document diagnostics" },
      { "<Leader>sD", "<Cmd>Telescope diagnostics<CR>", desc = "Workspace diagnostics" },
      { "<Leader>sg", Util.pick("live_grep"), desc = "Grep (root)" },
      { "<Leader>sG", Util.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
      { "<Leader>sh", "<Cmd>Telescope help_tags<CR>", desc = "Help pages" },
      { "<Leader>sH", "<Cmd>Telescope highlights<CR>", desc = "Search highlight groups" },
      { "<Leader>sj", "<Cmd>Telescope jumplist<CR>", desc = "Jumplist" },
      { "<Leader>sk", "<Cmd>Telescope keymaps<CR>", desc = "Key maps" },
      { "<Leader>sl", "<Cmd>Telescope loclist<CR>", desc = "Location list" },
      { "<Leader>sM", "<Cmd>Telescope man_pages<CR>", desc = "Man pages" },
      { "<Leader>sm", "<Cmd>Telescope marks<CR>", desc = "Jump to mark" },
      { "<Leader>so", "<Cmd>Telescope vim_options<CR>", desc = "Options" },
      { "<Leader>sR", "<Cmd>Telescope resume<CR>", desc = "Resume" },
      { "<Leader>sq", "<Cmd>Telescope quickfix<CR>", desc = "Quickfix list" },
      { "<Leader>sw", Util.pick("grep_string", { word_match = "-w" }), desc = "Word (root)" },
      { "<Leader>sW", Util.pick("grep_string", { root = false, word_match = "-w" }), desc = "Word (cwd)" },
      { "<Leader>sw", Util.pick("grep_string"), mode = "v", desc = "Selection (Root Dir)" },
      { "<Leader>sW", Util.pick("grep_string", { root = false }), mode = "v", desc = "Selection (cwd)" },
      { "<Leader>uC", Util.pick("colorscheme", { enable_preview = true }), desc = "Colorscheme with preview" },
      {
        "<Leader>ss",
        function()
          require("telescope.builtin").lsp_document_symbols({
            symbols = Util.config.get_kind_filter(),
          })
        end,
        desc = "Goto symbol",
      },
      {
        "<Leader>sS",
        function()
          require("telescope.builtin").lsp_dynamic_workspace_symbols({
            symbols = Util.config.get_kind_filter(),
          })
        end,
        desc = "Goto symbol (workspace)",
      },
    },
    opts = function()
      local actions = require("telescope.actions")
      local actions_layout = require("telescope.actions.layout")

      local open_with_trouble = function(...)
        return require("trouble.sources.telescope").open(...)
      end
      local find_files_no_ignore = function()
        local action_state = require("telescope.actions.state")
        local line = action_state.get_current_line()
        Util.pick("find_files", { no_ignore = true, default_text = line })()
      end
      local find_files_with_hidden = function()
        local action_state = require("telescope.actions.state")
        local line = action_state.get_current_line()
        Util.pick("find_files", { hidden = true, default_text = line })()
      end

      local function find_command()
        if 1 == vim.fn.executable("rg") then
          return { "rg", "--files", "--color", "never", "-g", "!.git" }
        elseif 1 == vim.fn.executable("fd") then
          return { "fd", "--type", "f", "--color", "never", "-E", ".git" }
        elseif 1 == vim.fn.executable("fdfind") then
          return { "fdfind", "--type", "f", "--color", "never", "-E", ".git" }
        elseif 1 == vim.fn.executable("find") and vim.fn.has("win32") == 0 then
          return { "find", ".", "-type", "f" }
        elseif 1 == vim.fn.executable("where") then
          return { "where", "/r", ".", "*" }
        end
      end

      return {
        defaults = {
          prompt_prefix = " ",
          selection_caret = " ",
          get_selection_window = function()
            local wins = vim.api.nvim_list_wins()
            table.insert(wins, 1, vim.api.nvim_get_current_win())
            for _, win in ipairs(wins) do
              local buf = vim.api.nvim_win_get_buf(win)
              if vim.bo[buf].buftype == "" then
                return win
              end
            end
            return 0
          end,
          mappings = {
            i = {
              ["<C-t>"] = open_with_trouble,
              ["<M-t>"] = open_with_trouble,
              ["<M-i>"] = find_files_no_ignore,
              ["<M-.>"] = find_files_with_hidden,
              ["<M-p>"] = actions_layout.toggle_preview,
              ["<C-Down>"] = actions.cycle_history_next,
              ["<C-Up>"] = actions.cycle_history_prev,
              ["<C-n>"] = actions.move_selection_next,
              ["<C-p>"] = actions.move_selection_previous,
              ["<C-j>"] = actions.move_selection_next,
              ["<C-k>"] = actions.move_selection_previous,
              ["<C-b>"] = { "<Left>", type = "command" },
              ["<C-f>"] = { "<Right>", type = "command" },
              ["<C-u>"] = false,
            },
            n = {
              ["q"] = actions.close,
            },
          },
        },
        pickers = {
          find_files = {
            find_command = find_command,
            hidden = true,
          },
        },
        extensions = {
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
                ["<C-y>"] = require("telescope-undo.actions").yank_additions,
                ["<M-y>"] = require("telescope-undo.actions").yank_deletions,
                ["<CR>"] = require("telescope-undo.actions").restore,
              },
            },
          },
          frecency = {
            auto_validate = false,
            show_unindexed = false,
            use_sqlite = false,
          },
        },
      }
    end,
  },

  {
    "nvim-telescope/telescope.nvim",
    optional = true,
    opts = function(_, opts)
      if not Util.plugin.has("flash.nvim") then
        return
      end
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
            local current_picker = require("telescope.actions.state").get_current_picker(prompt_bufnr)
            current_picker:set_selection(match.pos[1] - 1)
          end,
        })
      end
      opts.defaults = vim.tbl_deep_extend("force", opts.defaults or {}, {
        mappings = { n = { s = flash }, i = { ["<C-s>"] = flash } },
      })
    end,
  },

  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    keys = {
      { "<Leader>sa", Util.pick("live_grep_args"), desc = "Grep with args (root)" },
      { "<Leader>sA", Util.pick("live_grep_args", { root = false }), desc = "Grep with args (cwd)" },
    },
    config = function()
      require("telescope").load_extension("live_grep_args")
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
    "stevearc/dressing.nvim",
    lazy = true,
    enabled = function()
      return Util.pick.want() == "telescope"
    end,
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
    "neovim/nvim-lspconfig",
    opts = function()
      if Util.pick.want() ~= "telescope" then
        return
      end
      local Keys = require("plugins.lsp.keymaps").get()
      vim.list_extend(Keys, {
        {
          "gd",
          function()
            require("telescope.builtin").lsp_definitions({ reuse_win = true })
          end,
          desc = "Goto Definition",
          has = "definition",
        },
        { "gr", "<Cmd>Telescope lsp_references<CR>", desc = "References", nowait = true },
        {
          "gI",
          function()
            require("telescope.builtin").lsp_implementations({ reuse_win = true })
          end,
          desc = "Goto implementation",
        },
        {
          "gy",
          function()
            require("telescope.builtin").lsp_type_definitions({ reuse_win = true })
          end,
          desc = "Goto t[y]pe definition",
        },
      })
    end,
  },
}
