return {
  condition = function()
    return vim.opt.signcolumn:get() ~= "no"
  end,
  provider = "%s",
}
