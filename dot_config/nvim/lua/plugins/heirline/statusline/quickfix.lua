local conditions = require("heirline.conditions")
local icons = require("config.icons")

return {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "quickfix" },
    })
  end,
  require("heirline.utils").surround({ icons.powerline.block, icons.powerline.slant_right }, "dark_red", {
    hl = {
      fg = "fg",
      force = true,
    },
    require("plugins.heirline.components.quickfix.label"),
  }),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.quickfix.title"),
  require("plugins.heirline.components.align"),
  require("plugins.heirline.components.ruler"),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.scrollbar"),
}
