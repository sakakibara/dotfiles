return {
  hl = { fg = "blue", bold = true },
  flexible = 1,
  {
    provider = function(self)
      return self.reldirpath
    end,
  },
  {
    provider = function(self)
      return self.reldirpath and vim.fn.pathshorten(self.reldirpath, 5)
    end,
  },
  {
    provider = function(self)
      return self.reldirpath and vim.fn.pathshorten(self.reldirpath, 3)
    end,
  },
  {
    provider = function(self)
      return self.reldirpath and vim.fn.pathshorten(self.reldirpath)
    end,
  },
  { provider = "" },
  update = {
    "BufEnter",
    "DirChanged",
    "WinResized",
  },
}
