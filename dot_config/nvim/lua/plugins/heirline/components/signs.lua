local scutil = require("plugins.heirline.components.util.statuscolumn")
return {
  condition = function()
    return vim.opt.signcolumn:get() ~= "no"
  end,
  static = {
    handlers = {
      ["GitSignsTopdelete"] = scutil.gitsigns_click_handler,
      ["GitSignsUntracked"] = scutil.gitsigns_click_handler,
      ["GitSignsAdd"] = scutil.gitsigns_click_handler,
      ["GitSignsChangedelete"] = scutil.gitsigns_click_handler,
      ["GitSignsDelete"] = scutil.gitsigns_click_handler,
      ["DiagnosticSignError"] = scutil.diagnostics_click_handler,
      ["DiagnosticSignHint"] = scutil.diagnostics_click_handler,
      ["DiagnosticSignInfo"] = scutil.diagnostics_click_handler,
      ["DiagnosticSignWarn"] = scutil.diagnostics_click_handler,
      ["DapBreakpoint"] = scutil.dap_breakpoint_click_handler,
      ["DapBreakpointRejected"] = scutil.dap_breakpoint_click_handler,
      ["DapBreakpointCondition"] = scutil.dap_breakpoint_click_handler,
    },
  },
  provider = "%s",
  on_click = {
    name = "sign_click",
    callback = function(self, ...)
      local args = scutil.clickargs(self, ...)
      if args.sign and args.sign.name and self.handlers[args.sign.name] then
        self.handlers[args.sign.name](args)
      end
    end,
  },
}
