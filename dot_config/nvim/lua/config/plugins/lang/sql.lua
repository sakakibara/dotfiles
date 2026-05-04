-- lua/config/plugins/lang/sql.lua
local sql_ft = { "sql", "mysql", "plsql" }

local formatters_by_ft = {}
local linters_by_ft = {}
for _, ft in ipairs(sql_ft) do
  formatters_by_ft[ft] = { "sqlfluff" }
  linters_by_ft[ft] = { "sqlfluff" }
end

return Lib.lang.setup({
  mason = { "sqlfluff" },
  parsers = { "sql" },
  formatters = formatters_by_ft,
  formatters_setup = function(conform)
    conform.formatters = conform.formatters or {}
    conform.formatters.sqlfluff = {
      args = { "format", "--dialect=ansi", "-" },
    }
  end,
  linters = linters_by_ft,
  plugins = {
    {
      "tpope/vim-dadbod",
      cmd = "DB",
    },

    {
      "kristijanhusak/vim-dadbod-completion",
      dependencies = { "vim-dadbod" },
      ft = sql_ft,
    },

    {
      "kristijanhusak/vim-dadbod-ui",
      cmd = { "DBUI", "DBUIToggle", "DBUIAddConnection", "DBUIFindBuffer" },
      dependencies = { "vim-dadbod" },
      keys = {
        { "<Leader>D", "<Cmd>DBUIToggle<CR>", desc = "Toggle DBUI" },
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
  },
})
