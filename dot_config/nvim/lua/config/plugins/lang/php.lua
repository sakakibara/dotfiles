-- lua/config/plugins/lang/php.lua
if vim.fn.executable("php") == 0 then return {} end

Lib.mason.add("phpactor", "php-cs-fixer", "phpcs")
Lib.mason.add("php-debug-adapter")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "php" })
end)

Lib.plugin.on_load("nvim-dap", function()
  local dap = require("dap")
  local path = vim.fn.stdpath("data") .. "/mason/packages/php-debug-adapter"
  if not dap.adapters.php then
    dap.adapters.php = {
      type = "executable",
      command = "node",
      args = { path .. "/extension/out/phpDebug.js" },
    }
  end
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("phpactor", { capabilities = Lib.lsp.capabilities() })
  Lib.lsp.enable("phpactor")
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.php = { "php_cs_fixer" }
end)

Lib.plugin.on_load("nvim-lint", function()
  require("lint").linters_by_ft.php = { "phpcs" }
end)

return {}
