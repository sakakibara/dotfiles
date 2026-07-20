local plugins = Lib.lang.setup({
  cmd = "php",
  mason = { "phpactor", "php-cs-fixer", "phpcs", "php-debug-adapter" },
  parsers = { "php" },
  servers = { phpactor = {} },
  formatters = { php = { "php_cs_fixer" } },
  linters = { php = { "phpcs" } },
})

if vim.fn.executable("php") == 1 then
  -- DAP wiring lives outside the helper since dap configurations don't fit
  -- the helper's shape. Cmd-gated so it mirrors the helper's gate.
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
end

return plugins
