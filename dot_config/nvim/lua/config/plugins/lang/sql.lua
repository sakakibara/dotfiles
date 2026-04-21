-- lua/config/plugins/lang/sql.lua
local sql_ft = { "sql", "mysql", "plsql" }

Lib.mason.add("sqlfluff")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "sql" })
end)

Lib.plugin.on_load("conform.nvim", function()
  local conform = require("conform")
  conform.formatters = conform.formatters or {}
  conform.formatters.sqlfluff = {
    args = { "format", "--dialect=ansi", "-" },
  }
  for _, ft in ipairs(sql_ft) do
    conform.formatters_by_ft[ft] = conform.formatters_by_ft[ft] or {}
    table.insert(conform.formatters_by_ft[ft], "sqlfluff")
  end
end)

Lib.plugin.on_load("nvim-lint", function()
  local lint = require("lint")
  for _, ft in ipairs(sql_ft) do
    lint.linters_by_ft[ft] = lint.linters_by_ft[ft] or {}
    table.insert(lint.linters_by_ft[ft], "sqlfluff")
  end
end)

return {
  {
    "tpope/vim-dadbod",
    name = "vim-dadbod",
    cmd = "DB",
  },

  {
    "kristijanhusak/vim-dadbod-completion",
    name = "vim-dadbod-completion",
    dependencies = { "vim-dadbod" },
    ft = sql_ft,
  },

  {
    "kristijanhusak/vim-dadbod-ui",
    name = "vim-dadbod-ui",
    cmd = { "DBUI", "DBUIToggle", "DBUIAddConnection", "DBUIFindBuffer" },
    dependencies = { "vim-dadbod" },
    keys = {
      { "<Leader>du", "<Cmd>DBUIToggle<CR>", desc = "Toggle DBUI" },
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
}
