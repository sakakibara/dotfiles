return {
  condition = function()
    return vim.wo.spell
  end,
  provider = function()
    return "󰓆 " .. vim.o.spelllang .. " "
  end,
  hl = { bold = true, fg = "green" },
}
