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

T.describe("lib.lsp.enable", function()
  local function reset()
    package.loaded["lib.lsp"] = nil
    local lsp = require("lib.lsp")
    lsp._pending_enable = {}
    return lsp
  end

  T.it("calls vim.lsp.enable when binary is executable", function()
    local lsp = reset()
    local enabled = {}
    local orig = vim.lsp.enable
    vim.lsp.enable = function(name) enabled[#enabled + 1] = name end
    vim.lsp.config("test_enable_ok", { cmd = { "/bin/sh" } })
    lsp.enable("test_enable_ok")
    vim.lsp.enable = orig
    T.eq(enabled, { "test_enable_ok" })
  end)

  T.it("skips vim.lsp.enable and queues when binary is missing", function()
    local lsp = reset()
    local enabled = {}
    local orig = vim.lsp.enable
    vim.lsp.enable = function(name) enabled[#enabled + 1] = name end
    vim.lsp.config("test_enable_missing", { cmd = { "/nonexistent/binary-xyz" } })
    lsp.enable("test_enable_missing")
    vim.lsp.enable = orig
    T.eq(enabled, {})
    T.truthy(lsp._pending_enable["test_enable_missing"])
  end)

  T.it("MasonToolsUpdateCompleted re-enables a pending server once binary exists", function()
    local lsp = reset()
    local enabled = {}
    local orig = vim.lsp.enable
    vim.lsp.enable = function(name) enabled[#enabled + 1] = name end

    vim.lsp.config("test_enable_pending", { cmd = { "/nonexistent/pending-xyz" } })
    lsp.enable("test_enable_pending")
    T.eq(enabled, {})

    -- Pretend mason installed it: swap cmd to an executable path, fire event
    vim.lsp.config("test_enable_pending", { cmd = { "/bin/sh" } })
    vim.api.nvim_exec_autocmds("User", { pattern = "MasonToolsUpdateCompleted" })

    vim.lsp.enable = orig
    T.eq(enabled, { "test_enable_pending" })
    T.eq(lsp._pending_enable["test_enable_pending"], nil)
  end)

  T.it("never queues when immediately enabled", function()
    local lsp = reset()
    local orig = vim.lsp.enable
    vim.lsp.enable = function(_) end
    vim.lsp.config("test_enable_noop", { cmd = { "/bin/sh" } })
    lsp.enable("test_enable_noop")
    vim.lsp.enable = orig
    T.eq(lsp._pending_enable["test_enable_noop"], nil)
  end)

  T.it("function cmd without opts is queued (safe default, no spam)", function()
    local lsp = reset()
    local enabled = {}
    local orig = vim.lsp.enable
    vim.lsp.enable = function(name) enabled[#enabled + 1] = name end
    vim.lsp.config("test_enable_fn", { cmd = function() end })
    lsp.enable("test_enable_fn")
    vim.lsp.enable = orig
    T.eq(enabled, {})
    T.truthy(lsp._pending_enable["test_enable_fn"], "function-cmd without hint must defer, not trust")
  end)

  T.it("opts.cmd provides an explicit binary for availability check", function()
    local lsp = reset()
    local enabled = {}
    local orig = vim.lsp.enable
    vim.lsp.enable = function(name) enabled[#enabled + 1] = name end
    vim.lsp.config("test_enable_explicit", { cmd = function() end })
    lsp.enable("test_enable_explicit", { cmd = "/bin/sh" })
    vim.lsp.enable = orig
    T.eq(enabled, { "test_enable_explicit" })
  end)

  T.it("opts.cmd missing binary defers", function()
    local lsp = reset()
    local enabled = {}
    local orig = vim.lsp.enable
    vim.lsp.enable = function(name) enabled[#enabled + 1] = name end
    vim.lsp.config("test_enable_explicit_missing", { cmd = function() end })
    lsp.enable("test_enable_explicit_missing", { cmd = "/nonexistent/explicit-xyz" })
    vim.lsp.enable = orig
    T.eq(enabled, {})
    T.truthy(lsp._pending_enable["test_enable_explicit_missing"])
  end)
end)
