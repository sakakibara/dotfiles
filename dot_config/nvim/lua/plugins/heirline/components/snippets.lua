return {
  condition = function()
    return vim.tbl_contains({ "s", "i" }, vim.fn.mode())
  end,
  provider = function()
    if vim.snippet.active({ direction = 1 }) or vim.snippet.active({ direction = -1 }) then
      return " "
    end
  end,
  hl = { fg = "red", bold = true },
}
