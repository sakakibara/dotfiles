local upath = require("util.path")

return {
  init = function(self)
    self.fpath = upath.get_current_file_path()
    _, self.reldirpath, self.basename = upath.get_path_segments(self.fpath)
  end,
  require("plugins.heirline.components.breadcrumbs.dir_segments"),
  require("plugins.heirline.components.file.icon"),
  require("plugins.heirline.components.file.basename"),
  require("plugins.heirline.components.file.modified"),
  require("plugins.heirline.components.file.readonly"),
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
