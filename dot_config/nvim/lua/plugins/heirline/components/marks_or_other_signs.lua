return {
  condition = function()
    return vim.opt.signcolumn:get() ~= "no"
  end,
  static = {
    handlers = {
      ["DiagnosticSignError"] = Util.ui.diagnostics_click_handler,
      ["DiagnosticSignHint"] = Util.ui.diagnostics_click_handler,
      ["DiagnosticSignInfo"] = Util.ui.diagnostics_click_handler,
      ["DiagnosticSignWarn"] = Util.ui.diagnostics_click_handler,
      ["DapBreakpoint"] = Util.ui.dap_breakpoint_click_handler,
      ["DapBreakpointRejected"] = Util.ui.dap_breakpoint_click_handler,
      ["DapBreakpointCondition"] = Util.ui.dap_breakpoint_click_handler,
    },
  },
  provider = function(self)
    local buf = self.bufnr or vim.api.nvim_get_current_buf()
    local signs = Util.ui.get_signs(buf, vim.v.lnum)
    local sign
    for _, s in ipairs(signs) do
      if s.name and not (s.name:find("GitSign") or s.name:find("MiniDiffSign")) then
        sign = s
      end
    end
    return Util.ui.icon(Util.ui.get_mark(buf, vim.v.lnum) or sign)
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
