local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "nofile", "prompt", "help" },
    })
  end,
  require("plugins.heirline.components.file.type"),
  require("plugins.heirline.components.align"),
}
