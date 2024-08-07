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
      ["DiagnosticSignError"] = Util.ui.diagnostics_click_handler,
      ["DiagnosticSignHint"] = Util.ui.diagnostics_click_handler,
      ["DiagnosticSignInfo"] = Util.ui.diagnostics_click_handler,
      ["DiagnosticSignWarn"] = Util.ui.diagnostics_click_handler,
      ["DapBreakpoint"] = Util.ui.dap_breakpoint_click_handler,
      ["DapBreakpointRejected"] = Util.ui.dap_breakpoint_click_handler,
      ["DapBreakpointCondition"] = Util.ui.dap_breakpoint_click_handler,
    },
  },
  provider = "%s",
  on_click = {
    name = "sign_click",
    callback = function(self, ...)
      local args = Util.ui.click_args(self, ...)
      if args.sign and args.sign.name and self.handlers[args.sign.name] then
        self.handlers[args.sign.name](args)
      end
    end,
  },
}
