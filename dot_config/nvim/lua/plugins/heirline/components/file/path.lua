local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "" },
      filetype = { "oil" },
    })
  end,
  init = function(self)
    self.fpath = Util.path.get_current_file_path()
    self.pwd, self.reldirpath, self.basename = Util.path.format_path(self.fpath, {
      replace_home = true,
      return_segments = true,
      last_separator = true,
    })
  end,
  require("plugins.heirline.components.file.work_dir"),
  require("plugins.heirline.components.file.dir_path"),
  require("plugins.heirline.components.file.basename"),
  update = {
    "BufEnter",
    "BufWritePost",
    "DirChanged",
    "WinResized",
    "TextChanged",
    "InsertLeave",
    "BufModifiedSet",
  },
}
