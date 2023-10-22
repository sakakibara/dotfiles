local conditions = require("heirline.conditions")

return {
  condition = conditions.is_not_active,
  {
    hl = {
      fg = "gray",
      force = true,
    },
    require("plugins.heirline.components.file.path"),
  },
  require("plugins.heirline.components.truncate"),
  require("plugins.heirline.components.align"),
}
