local conditions = require("heirline.conditions")
local icons = require("config.icons")

return {
  condition = conditions.has_diagnostics,
  update = { "DiagnosticChanged", "BufEnter" },
  on_click = {
    callback = function()
      require("trouble").toggle({ mode = "document_diagnostics" })
    end,
    name = "heirline_diagnostics",
  },
  static = {},
  init = function(self)
    self.errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
    self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
    self.hints = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
    self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
  end,
  {
    provider = function(self)
      return self.errors > 0 and (icons.diagnostics.Error .. self.errors .. " ")
    end,
    hl = "DiagnosticError",
  },
  {
    provider = function(self)
      return self.warnings > 0 and (icons.diagnostics.Warn .. self.warnings .. " ")
    end,
    hl = "DiagnosticWarn",
  },
  {
    provider = function(self)
      return self.info > 0 and (icons.diagnostics.Info .. self.info .. " ")
    end,
    hl = "DiagnosticInfo",
  },
  {
    provider = function(self)
      return self.hints > 0 and (icons.diagnostics.Hint .. self.hints .. " ")
    end,
    hl = "DiagnosticHint",
  },
}
