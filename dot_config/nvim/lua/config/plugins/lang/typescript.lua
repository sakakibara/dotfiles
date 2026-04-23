-- lua/config/plugins/lang/typescript.lua
if vim.fn.executable("node") == 0 then return {} end

Lib.mason.add("vtsls")
Lib.mason.add("js-debug-adapter")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "typescript", "tsx" })
end)

Lib.plugin.on_load("nvim-dap", function()
  local dap = require("dap")
  if not dap.adapters["pwa-node"] then
    dap.adapters["pwa-node"] = {
      type = "server",
      host = "localhost",
      port = "${port}",
      executable = {
        command = "node",
        args = {
          vim.fn.stdpath("data") .. "/mason/packages/js-debug-adapter/js-debug/src/dapDebugServer.js",
          "${port}",
        },
      },
    }
  end
  if not dap.adapters["node"] then
    dap.adapters["node"] = function(cb, config)
      if config.type == "node" then
        config.type = "pwa-node"
      end
      local nativeAdapter = dap.adapters["pwa-node"]
      if type(nativeAdapter) == "function" then
        nativeAdapter(cb, config)
      else
        cb(nativeAdapter)
      end
    end
  end

  local js_filetypes = { "typescript", "javascript", "typescriptreact", "javascriptreact" }

  local vscode = require("dap.ext.vscode")
  vscode.type_to_filetypes["node"] = js_filetypes
  vscode.type_to_filetypes["pwa-node"] = js_filetypes

  for _, language in ipairs(js_filetypes) do
    if not dap.configurations[language] then
      dap.configurations[language] = {
        {
          type = "pwa-node",
          request = "launch",
          name = "Launch file",
          program = "${file}",
          cwd = "${workspaceFolder}",
        },
        {
          type = "pwa-node",
          request = "attach",
          name = "Attach",
          processId = require("dap.utils").pick_process,
          cwd = "${workspaceFolder}",
        },
      }
    end
  end
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  local ts_settings = {
    updateImportsOnFileMove = { enabled = "always" },
    suggest = {
      completeFunctionCalls = true,
    },
    inlayHints = {
      enumMemberValues = { enabled = true },
      functionLikeReturnTypes = { enabled = true },
      parameterNames = { enabled = "literals" },
      parameterTypes = { enabled = true },
      propertyDeclarationTypes = { enabled = true },
      variableTypes = { enabled = false },
    },
  }

  vim.lsp.config("vtsls", {
    capabilities = Lib.lsp.capabilities(),
    filetypes = {
      "javascript",
      "javascriptreact",
      "javascript.jsx",
      "typescript",
      "typescriptreact",
      "typescript.tsx",
    },
    settings = {
      complete_function_calls = true,
      vtsls = {
        enableMoveToFileCodeAction = true,
        autoUseWorkspaceTsdk = true,
        experimental = {
          maxInlayHintLength = 30,
          completion = {
            enableServerSideFuzzyMatch = true,
          },
        },
      },
      typescript = ts_settings,
      javascript = ts_settings,
    },
  })
  Lib.lsp.enable("vtsls")
end)

Lib.plugin.on_load("conform.nvim", function()
  local conform = require("conform")
  conform.formatters_by_ft.typescript = { "prettier" }
  conform.formatters_by_ft.typescriptreact = { "prettier" }
  conform.formatters_by_ft.javascript = { "prettier" }
  conform.formatters_by_ft.javascriptreact = { "prettier" }
end)

-- NOTE: LSP keymaps (gD goToSourceDefinition, gR file references, <Leader>co
-- organize imports, <Leader>cM add missing, <Leader>cu remove unused,
-- <Leader>cD fix all, <Leader>cV select TS version) and the custom
-- `_typescript.moveToFileRefactoring` command handler dropped — Util.lsp
-- helpers not ported to M2 framework.

return {
  {
    "mxsdev/nvim-dap-vscode-js",
    name = "nvim-dap-vscode-js",
    ft = { "javascript", "typescript", "typescriptreact", "javascriptreact" },
    dependencies = { "nvim-dap" },
    opts = {
      adapters = { "pwa-node", "pwa-chrome", "pwa-msedge", "node-terminal", "pwa-extensionHost", "node", "chrome" },
      debugger_path = vim.fn.stdpath("data") .. "/mason/packages/js-debug-adapter/js-debug",
    },
  },
}
