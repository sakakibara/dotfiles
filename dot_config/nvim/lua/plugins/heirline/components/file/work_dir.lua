return {
  init = function(self)
    self.icon = (vim.fn.haslocaldir(0) == 1 and Util.config.icons.status.DirectoryAlt or Util.config.icons.status.Directory)
  end,
  hl = { fg = "gray", bold = true },
  on_click = {
    callback = function()
      require("oil").open()
    end,
    name = "heirline_workdir",
  },
  flexible = 1,
  {
    provider = function(self)
      return self.icon .. self.pwd
    end,
  },
  {
    provider = function(self)
      return self.icon .. vim.fn.pathshorten(self.pwd)
    end,
  },
  { provider = "" },
  update = {
    "BufEnter",
    "DirChanged",
    "WinResized",
  },
}
