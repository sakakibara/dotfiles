return {
  condition = function()
    return vim.bo.modified
  end,
  provider = " [+]",
  hl = { fg = "green" },
  update = {
    "TextChanged",
    "InsertLeave",
    "BufModifiedSet",
  },
}
