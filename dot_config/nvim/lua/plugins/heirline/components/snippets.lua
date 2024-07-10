return {
  condition = function()
    return vim.tbl_contains({ "s", "i" }, vim.fn.mode())
  end,
  provider = function()
    local luasnip = require("luasnip")
    if luasnip.jumpable(1) or luasnip.jumpable(-1) then
      return " "
    end
  end,
  hl = { fg = "red", bold = true },
}
