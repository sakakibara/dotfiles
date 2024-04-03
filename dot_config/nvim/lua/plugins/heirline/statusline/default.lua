local conditions = require("heirline.conditions")

return {
  require("plugins.heirline.components.cap"),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.vi_mode"),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.git"),
  {
    condition = function()
      return not conditions.buffer_matches({
        buftype = { "acwrite", "nofile", "prompt", "help" },
      })
    end,
    require("plugins.heirline.components.file.path"),
    require("plugins.heirline.components.file.modified"),
    require("plugins.heirline.components.file.readonly"),
  },
  require("plugins.heirline.components.truncate"),
  require("plugins.heirline.components.align"),
  require("plugins.heirline.components.spell"),
  require("plugins.heirline.components.task"),
  require("plugins.heirline.components.diagnostics"),
  require("plugins.heirline.components.dap"),
  require("plugins.heirline.components.lsp"),
  require("plugins.heirline.components.file.icon"),
  require("plugins.heirline.components.file.type"),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.file.encoding"),
  require("plugins.heirline.components.file.format"),
  require("plugins.heirline.components.ruler"),
  require("plugins.heirline.components.search_count"),
  require("plugins.heirline.components.space"),
  require("plugins.heirline.components.scrollbar"),
}
