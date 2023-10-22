return {
  condition = function()
    ---@diagnostic disable-next-line: undefined-field
    return vim.v.hlsearch ~= 0 and vim.o.cmdheight == 0
  end,
  init = function(self)
    local ok, search = pcall(vim.fn.searchcount)
    ---@diagnostic disable-next-line: undefined-field
    if ok and search.total then
      self.search = search
    end
  end,
  provider = function(self)
    local search = self.search
    return string.format(" %d/%d", search.current, math.min(search.total, search.maxcount))
  end,
  hl = { fg = "purple", bold = true },
}
