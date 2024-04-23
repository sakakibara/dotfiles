return {
  provider = function()
    if vim.bo.bomb then
      return "BOM "
    end
  end,
}
