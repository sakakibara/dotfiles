local sql_ft = { "sql", "mysql", "plsql" }

return {
  {
    "tpope/vim-dadbod",
    cmd = "DB",
  },

  {
    "kristijanhusak/vim-dadbod-completion",
    dependencies = "vim-dadbod",
    ft = sql_ft,
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern = sql_ft,
        callback = function()
          local cmp = require("cmp")

          local sources = vim.tbl_map(function(source)
            return { name = source.name }
          end, cmp.get_config().sources)

          table.insert(sources, { name = "vim-dadbod-completion" })

          cmp.setup.buffer({ sources = sources })
        end,
      })
    end,
  },

  {
    "kristijanhusak/vim-dadbod-ui",
    cmd = { "DBUI", "DBUIToggle", "DBUIAddConnection", "DBUIFindBuffer" },
    dependencies = "vim-dadbod",
    keys = {
      { "<Leader>du", "<cmd>DBUIToggle<CR>", desc = "Toggle DBUI" },
    },
    init = function()
      local data_path = vim.fn.stdpath("data")

      vim.g.db_ui_auto_execute_table_helpers = 1
      vim.g.db_ui_save_location = data_path .. "/dadbod_ui"
      vim.g.db_ui_show_database_icon = true
      vim.g.db_ui_tmp_query_location = data_path .. "/dadbod_ui/tmp"
      vim.g.db_ui_use_nerd_fonts = true
      vim.g.db_ui_use_nvim_notify = true
      vim.g.db_ui_execute_on_save = false
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    optional = true,
    opts = { ensure_installed = { "sql" } },
  },

  {
    "mason-org/mason.nvim",
    opts = { ensure_installed = { "sqlfluff" } },
  },

  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = function(_, opts)
      for _, ft in ipairs(sql_ft) do
        opts.linters_by_ft[ft] = opts.linters_by_ft[ft] or {}
        table.insert(opts.linters_by_ft[ft], "sqlfluff")
      end
    end,
  },

  {
    "stevearc/conform.nvim",
    optional = true,
    opts = function(_, opts)
      opts.formatters.sqlfluff = {
        args = { "format", "--dialect=ansi", "-" },
      }
      for _, ft in ipairs(sql_ft) do
        opts.formatters_by_ft[ft] = opts.formatters_by_ft[ft] or {}
        table.insert(opts.formatters_by_ft[ft], "sqlfluff")
      end
    end,
  },
}
