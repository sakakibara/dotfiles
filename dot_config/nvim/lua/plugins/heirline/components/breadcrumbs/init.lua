return {
  init = function(self)
    self.fpath = Util.path.get_current_file_path()
    _, self.reldirpath, self.basename = Util.path.format_path(self.fpath, {
      replace_home = true,
      return_segments = true,
      last_separator = true,
    })
  end,
  require("plugins.heirline.components.breadcrumbs.dir_segments"),
  require("plugins.heirline.components.file.icon"),
  require("plugins.heirline.components.file.basename"),
  require("plugins.heirline.components.file.modified"),
  require("plugins.heirline.components.file.readonly"),
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
