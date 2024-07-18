local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({ buftype = { "terminal" } })
  end,
  require("plugins.heirline.components.file.type"),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.terminal_name"),
  require("plugins.heirline.components.align"),
}
