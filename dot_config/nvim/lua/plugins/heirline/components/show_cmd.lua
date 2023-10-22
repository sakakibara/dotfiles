return {
  condition = function()
    return vim.o.cmdheight == 0
  end,
  provider = "%3.5(%S%)",
  hl = function(self)
    return { bold = true, fg = self:mode_color() }
  end,
}
