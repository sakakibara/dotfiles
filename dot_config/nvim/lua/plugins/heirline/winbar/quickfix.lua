local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "quickfix" },
    })
  end,
  require("plugins.heirline.components.quickfix.title"),
  require("plugins.heirline.components.align"),
}
