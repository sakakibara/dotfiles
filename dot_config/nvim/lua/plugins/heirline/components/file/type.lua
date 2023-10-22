return {
  provider = function()
    return string.lower(vim.bo.filetype)
  end,
  hl = "Type",
  update = "FileType",
}
