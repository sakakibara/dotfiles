local FbUtil = require("util.telescope-file-browser")

return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-live-grep-args.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
      },
    },
    cmd = "Telescope",
    version = false,
    keys = {
      { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
      { "<leader>ff", FbUtil.file_browser, desc = "Find Files" },
      { "<leader>fF", "<cmd>Telescope find_files hidden=true<cr>", desc = "Find Files" },
      { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Recent" },
      { "<leader>:", "<cmd>Telescope commands<cr>", desc = "Commands" },
      { "<leader>/", "<cmd>Telescope live_grep_args<cr>", desc = "Grep" },
    },
    opts = {
      defaults = {
        preview = { hide_on_startup = true },
        prompt_prefix = " ",
        selection_caret = " ",
        mappings = {
          i = {
            ["<c-t>"] = function(...)
              return require("trouble.providers.telescope").open_with_trouble(...)
            end,
            ["<a-i>"] = function()
              Util.telescope("find_files", { no_ignore = true })()
            end,
            ["<a-h>"] = function()
              Util.telescope("find_files", { hidden = true })()
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
            ["<Esc>"] = function(...)
              return require("telescope.actions").close(...)
            end,
            ["jk"] = function(...)
              return require("telescope.actions").close(...)
            end,
            ["<M-p>"] = function(...)
              return require("telescope.actions.layout").toggle_preview(...)
            end,
          },
          n = {
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
      },
    },
    config = function(_, opts)
      require("telescope").setup(opts)
      require("telescope").load_extension("live_grep_args")
      require("telescope").load_extension("file_browser")
      require("telescope").load_extension("fzf")
    end
  },
}
