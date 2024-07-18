local conditions = require("heirline.conditions")

return {
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
  require("plugins.heirline.components.align"),
}
