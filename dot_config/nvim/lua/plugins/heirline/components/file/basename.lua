return {
  provider = function(self)
    return self.basename
  end,
  hl = function()
    if vim.bo.modified then
      return { fg = "bright_fg", bold = true, italic = true }
    end
    return "bright_fg"
  end,
  update = {
    "BufWinEnter",
    "TextChanged",
    "InsertLeave",
    "BufModifiedSet",
  },
}
