local conditions = require("heirline.conditions")

return {
  fallthrough = false,
  {
    condition = function()
      return conditions.buffer_matches({ buftype = { "terminal" } })
    end,
    {
      require("plugins.heirline.components.file.type"),
      require("plugins.heirline.components.space"),
      require("plugins.heirline.components.terminal_name"),
    },
  },
  {
    fallthrough = false,
    {
      condition = conditions.is_not_active,
      {
        hl = { fg = "gray", force = true },
        require("plugins.heirline.components.breadcrumbs"),
        require("plugins.heirline.components.truncate"),
      },
    },
    {
      require("plugins.heirline.components.breadcrumbs"),
      require("plugins.heirline.components.truncate"),
    },
  },
  require("plugins.heirline.components.align"),
}
