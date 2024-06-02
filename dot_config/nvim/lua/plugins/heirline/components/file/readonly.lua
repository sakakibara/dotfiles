return {
  {
    condition = function()
      return not vim.bo.modifiable or vim.bo.readonly
    end,
    provider = " " .. Util.config.icons.status.Lock,
    hl = { fg = "orange" },
    update = "BufReadPost",
  },
}
