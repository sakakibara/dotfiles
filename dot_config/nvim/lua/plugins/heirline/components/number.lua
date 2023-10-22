return {
  condition = function()
    return vim.opt.number:get() or vim.opt.relativenumber:get()
  end,
  provider = function(self)
    local num, relnum = vim.opt.number:get(), vim.opt.relativenumber:get()
    local lnum, rnum, virtnum = vim.v.lnum, vim.v.relnum, vim.v.virtnum
    local signs = vim.opt.signcolumn:get():find("nu")
      and vim.fn.sign_getplaced(self.bufnr or vim.api.nvim_get_current_buf(), { group = "*", lnum = lnum })[1].signs
    local str
    if virtnum ~= 0 then
      str = "%="
    elseif signs and #signs > 0 then
      local sign = vim.fn.sign_getdefined(signs[1].name)[1]
      str = "%=%#" .. sign.texthl .. "#" .. sign.text .. "%*"
    elseif not num and not relnum then
      str = "%="
    else
      local cur = relnum and (rnum > 0 and rnum or (num and lnum or 0)) or lnum
      str = (rnum == 0 and relnum) and cur .. "%=" or "%=" .. cur
    end
    return str
  end,
}
