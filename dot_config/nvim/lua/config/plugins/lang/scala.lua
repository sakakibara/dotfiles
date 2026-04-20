-- lua/config/plugins/lang/scala.lua
if vim.fn.executable("scala") == 0 then return {} end

-- metals: install via `coursier install metals` (not auto-managed; mason has
-- partial scala coverage but coursier is the standard path for Scala users).

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "scala" })
end)

-- nvim-metals owns its own initialize_or_attach lifecycle; do not register
-- metals with nvim-lspconfig.

return {
  {
    "scalameta/nvim-metals",
    name = "nvim-metals",
    ft = { "scala", "sbt" },
    config = function()
      local metals = require("metals")
      local metals_config = metals.bare_config()
      metals_config.capabilities = Lib.lsp.capabilities()
      metals_config.init_options = metals_config.init_options or {}
      metals_config.init_options.statusBarProvider = "off"
      metals_config.settings = {
        showImplicitArguments = true,
        excludedPackages = { "akka.actor.typed.javadsl", "com.github.swagger.akka.javadsl" },
      }
      -- Wire metals to nvim-dap at LspAttach time (nvim-metals provides the
      -- DAP adapter; no external package needed).
      if Lib.plugin.has("nvim-dap") then
        metals_config.on_attach = metals.setup_dap
      end

      local group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "scala", "sbt" },
        group = group,
        callback = function()
          metals.initialize_or_attach(metals_config)
        end,
      })

      -- Useful non-DAP metals keymap.
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "scala", "sbt" },
        group = group,
        callback = function(args)
          vim.keymap.set("n", "<Leader>mc", function()
            require("metals").compile_cascade()
          end, { buffer = args.buf, desc = "Metals Compile Cascade" })
        end,
      })
    end,
  },
}
