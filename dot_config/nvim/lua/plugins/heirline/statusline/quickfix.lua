local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "quickfix" },
    })
  end,
  require("plugins.heirline.components.vi_mode"),
  require("plugins.heirline.components.macro_rec"),
  require("plugins.heirline.components.space"),
  require("heirline.utils").surround(
    { Util.config.icons.powerline.block, Util.config.icons.powerline.block },
    "gray",
    {
      hl = {
        fg = false,
        force = true,
      },
      require("plugins.heirline.components.quickfix.label"),
    }
  ),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.quickfix.title"),
  require("plugins.heirline.components.align"),
  require("plugins.heirline.components.ruler"),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.scrollbar"),
}
