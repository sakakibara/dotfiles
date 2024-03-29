local conditions = require("heirline.conditions")
local icons = require("config.icons")

return {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "nofile", "prompt", "help" },
    })
  end,
  require("heirline.utils").surround({ icons.powerline.block, icons.powerline.block }, "gray", {
    hl = {
      fg = false,
      force = true,
    },
    require("plugins.heirline.components.file.type"),
  }),
  { provider = "%q" },
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.file.help_filename"),
  require("plugins.heirline.components.align"),
}
