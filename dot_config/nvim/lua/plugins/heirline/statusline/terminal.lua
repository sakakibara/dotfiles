local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({ buftype = { "terminal" } })
  end,
  hl = { bg = "dark_red" },
  {
    condition = conditions.is_active,
    require("plugins.heirline.components.vi_mode"),
    require("plugins.heirline.components.space"),
  },
  require("plugins.heirline.components.file.path"),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.terminal_name"),
  require("plugins.heirline.components.align"),
}
