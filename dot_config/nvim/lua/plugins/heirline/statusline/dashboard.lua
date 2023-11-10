local conditions = require("heirline.conditions")
local upath = require("util.path")

return {
  condition = function()
    return conditions.buffer_matches({
      filetype = { "dashboard" },
    })
  end,
  {
    init = function(self)
      self.fpath = upath.get_current_file_path()
      self.pwd = upath.get_path_segments(self.fpath)
    end,
    require("plugins.heirline.components.file.work_dir"),
  },
  require("plugins.heirline.components.align"),
  require("plugins.heirline.components.file.type"),
}
