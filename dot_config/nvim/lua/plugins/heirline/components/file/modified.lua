return {
  condition = function()
    return vim.bo.modified
  end,
  provider = " [+]",
  hl = { fg = "red" },
  update = {
    "TextChanged",
    "InsertLeave",
    "BufModifiedSet",
  },
}
