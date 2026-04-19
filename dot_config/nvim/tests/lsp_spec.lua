local T = require("tests.helpers")

local function reset_lsp()
  package.loaded["lib.lsp"] = nil
  return require("lib.lsp")
end

T.describe("lib.lsp", function()
  T.it("capabilities returns a table with textDocument.completion", function()
    local lsp = reset_lsp()
    local caps = lsp.capabilities()
    T.truthy(caps.textDocument)
    T.truthy(caps.textDocument.completion)
  end)

  T.it("is_enabled(nil) returns false safely", function()
    local lsp = reset_lsp()
    T.eq(lsp.is_enabled(99999), false)  -- non-existent buffer
  end)

  T.it("on_attach stores callback for later LspAttach events", function()
    local lsp = reset_lsp()
    local registered = false
    lsp.on_attach(function() registered = true end)
    -- simulate LspAttach by directly calling _fire_attach
    lsp._fire_attach({ buf = vim.api.nvim_get_current_buf(), data = { client_id = 1 } })
    T.truthy(registered)
  end)
end)
