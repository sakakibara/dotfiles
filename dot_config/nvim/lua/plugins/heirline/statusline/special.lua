local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "nofile", "prompt", "help", "quickfix" },
    })
  end,
  require("plugins.heirline.components.file.type"),
  { provider = "%q" },
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.file.help_filename"),
  require("plugins.heirline.components.align"),
}
