return {
  {
    provider = function()
      local tname, _ = vim.api.nvim_buf_get_name(0):gsub(".*:", "")
      return " " .. tname
    end,
    hl = { fg = "blue", bold = true },
  },
  { provider = " - " },
  {
    provider = function()
      ---@diagnostic disable-next-line: undefined-field
      return vim.b.term_title
    end,
  },
}
