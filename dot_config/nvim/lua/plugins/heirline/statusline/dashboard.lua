local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({
      filetype = { "dashboard" },
    })
  end,
  {
    init = function(self)
      self.fpath = Util.path.get_current_file_path()
      self.pwd = Util.path.get_path_segments(self.fpath)
    end,
    require("plugins.heirline.components.file.work_dir"),
  },
  require("plugins.heirline.components.align"),
  require("plugins.heirline.components.file.type"),
}
