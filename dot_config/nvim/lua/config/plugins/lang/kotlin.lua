-- lua/config/plugins/lang/kotlin.lua
local plugins = Lib.lang.setup({
  cmd = "kotlin",
  mason = { "kotlin-language-server", "ktlint", "kotlin-debug-adapter" },
  parsers = { "kotlin" },
  servers = { kotlin_language_server = {} },
  formatters = { kotlin = { "ktlint" } },
  linters = { kotlin = { "ktlint" } },
})

if vim.fn.executable("kotlin") == 1 then
  -- DAP wiring lives outside the helper since dap configurations don't fit
  -- the helper's shape. Cmd-gated so it mirrors the helper's gate.
  Lib.plugin.on_load("nvim-dap", function()
    local dap = require("dap")
    if not dap.adapters.kotlin then
      dap.adapters.kotlin = {
        type = "executable",
        command = "kotlin-debug-adapter",
        options = { auto_continue_if_many_stopped = false },
      }
    end

    dap.configurations.kotlin = {
      {
        type = "kotlin",
        request = "launch",
        name = "This file",
        mainClass = function()
          local root = vim.fs.find("src", { path = vim.uv.cwd(), upward = true, stop = vim.env.HOME })[1] or ""
          local fname = vim.api.nvim_buf_get_name(0)
          return fname:gsub(root, ""):gsub("main/kotlin/", ""):gsub(".kt", "Kt"):gsub("/", "."):sub(2, -1)
        end,
        projectRoot = "${workspaceFolder}",
        jsonLogFile = "",
        enableJsonLogging = false,
      },
      {
        type = "kotlin",
        request = "attach",
        name = "Attach to debugging session",
        port = 5005,
        args = {},
        projectRoot = vim.fn.getcwd,
        hostName = "localhost",
        timeout = 2000,
      },
    }
  end)
end

return plugins
