local upath = require("util.path")
local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({
      buftype = { "" },
      filetype = { "oil" },
    })
  end,
  init = function(self)
    self.fpath = upath.get_current_file_path()
    self.pwd, self.reldirpath, self.basename = upath.get_path_segments(self.fpath)
  end,
  require("plugins.heirline.components.file.work_dir"),
  require("plugins.heirline.components.file.dir_path"),
  require("plugins.heirline.components.file.basename"),
  update = {
    "BufWinEnter",
    "BufWritePost",
    "DirChanged",
    "WinResized",
    "TextChanged",
    "InsertLeave",
    "BufModifiedSet",
  },
}
