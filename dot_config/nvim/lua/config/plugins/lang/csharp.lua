-- lua/config/plugins/lang/csharp.lua
if vim.fn.executable("dotnet") == 0 then return {} end

Lib.mason.add("omnisharp", "csharpier")
Lib.mason.add("netcoredbg")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "c_sharp" })
end)

Lib.plugin.on_load("nvim-dap", function()
  local dap = require("dap")
  if not dap.adapters["netcoredbg"] then
    dap.adapters["netcoredbg"] = {
      type = "executable",
      command = vim.fn.exepath("netcoredbg"),
      args = { "--interpreter=vscode" },
    }
  end
  for _, lang in ipairs({ "cs", "fsharp", "vb" }) do
    if not dap.configurations[lang] then
      dap.configurations[lang] = {
        {
          type = "netcoredbg",
          name = "Launch file",
          request = "launch",
          program = function()
            return vim.fn.input("Path to dll: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
        },
      }
    end
  end
end)

Lib.neotest.add("neotest-dotnet", function() return require("neotest-dotnet") end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("omnisharp", {
    capabilities = Lib.lsp.capabilities(),
    handlers = {
      ["textDocument/definition"] = function(...)
        return require("omnisharp_extended").handler(...)
      end,
    },
    enable_roslyn_analyzers = true,
    organize_imports_on_format = true,
    enable_import_completion = true,
  })
  Lib.lsp.enable("omnisharp")
end)

-- Omnisharp-extended "Goto Definition" keymap.
Lib.lsp.on_attach(function(args)
  local client = vim.lsp.get_client_by_id(args.data and args.data.client_id)
  if not client or client.name ~= "omnisharp" then return end
  vim.keymap.set("n", "gd", function()
    require("omnisharp_extended").lsp_definitions()
  end, { buffer = args.buf, desc = "Goto Definition" })
end)

Lib.plugin.on_load("conform.nvim", function()
  local conform = require("conform")
  conform.formatters = conform.formatters or {}
  conform.formatters.csharpier = {
    command = vim.fn.executable("csharpier") == 1 and "csharpier" or "dotnet-csharpier",
    args = { "--write-stdout" },
  }
  conform.formatters_by_ft.cs = { "csharpier" }
end)

return {
  {
    "Hoffs/omnisharp-extended-lsp.nvim",
    name = "omnisharp-extended-lsp.nvim",
    ft = { "cs", "csproj", "razor" },
  },
  {
    "Issafalcon/neotest-dotnet",
    name = "neotest-dotnet",
    ft = "cs",
    dependencies = { "neotest" },
  },
}
