return {
  provider = function()
    return string.upper(vim.bo.filetype)
  end,
  hl = "Type",
  update = { "BufEnter", "FileType" },
}
