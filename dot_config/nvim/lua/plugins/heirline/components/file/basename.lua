return {
  provider = function(self)
    return self.basename
  end,
  hl = function()
    if vim.bo.modified then
      return { fg = "orange", bold = true, italic = true }
    end
    return "bright_fg"
  end,
  update = {
    "BufEnter",
    "TextChanged",
    "InsertLeave",
    "BufModifiedSet",
  },
}
