-- lua/config/plugins/lang/kotlin.lua
if vim.fn.executable("kotlin") == 0 then return {} end

Lib.mason.add("kotlin-language-server", "ktlint")
Lib.mason.add("kotlin-debug-adapter")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "kotlin" })
end)

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

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("kotlin_language_server", { capabilities = Lib.lsp.capabilities() })
  Lib.lsp.enable("kotlin_language_server")
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.kotlin = { "ktlint" }
end)

Lib.plugin.on_load("nvim-lint", function()
  require("lint").linters_by_ft.kotlin = { "ktlint" }
end)

return {}
