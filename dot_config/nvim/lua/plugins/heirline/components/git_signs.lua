return {
  condition = function()
    return vim.opt.signcolumn:get() ~= "no"
  end,
  static = {
    handlers = {
      ["GitSignsTopdelete"] = Util.ui.gitsigns_click_handler,
      ["GitSignsUntracked"] = Util.ui.gitsigns_click_handler,
      ["GitSignsAdd"] = Util.ui.gitsigns_click_handler,
      ["GitSignsChangedelete"] = Util.ui.gitsigns_click_handler,
      ["GitSignsDelete"] = Util.ui.gitsigns_click_handler,
    },
  },
  provider = function(self)
    local buf = self.bufnr or vim.api.nvim_get_current_buf()
    local is_file = vim.bo[buf].buftype == ""
    local signs = Util.ui.get_signs(buf, vim.v.lnum)
    local sign
    for _, s in ipairs(signs) do
      if s.name and s.name:find("GitSign") or s.name:find("MiniDiffSign") then
        sign = s
      end
    end
    return is_file and Util.ui.icon(sign) or ""
  end,
  on_click = {
    name = "mark_or_other_sign_click",
    callback = function(self, ...)
      local args = Util.ui.click_args(self, ...)
      if args.sign and args.sign.name and self.handlers[args.sign.name] then
        self.handlers[args.sign.name](args)
      end
    end,
  },
}
