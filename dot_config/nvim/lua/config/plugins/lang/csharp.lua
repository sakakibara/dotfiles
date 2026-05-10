local plugins = Lib.lang.setup({
  cmd = "dotnet",
  mason = { "omnisharp", "csharpier", "netcoredbg" },
  parsers = { "c_sharp" },
  servers = {
    omnisharp = {
      handlers = {
        ["textDocument/definition"] = function(...)
          return require("omnisharp_extended").handler(...)
        end,
      },
      enable_roslyn_analyzers = true,
      organize_imports_on_format = true,
      enable_import_completion = true,
      -- Omnisharp-extended "Goto Definition" keymap.
      on_attach = function(args, _)
        vim.keymap.set("n", "gd", function()
          require("omnisharp_extended").lsp_definitions()
        end, { buffer = args.buf, desc = "Goto definition" })
      end,
    },
  },
  formatters = { cs = { "csharpier" } },
  formatters_setup = function(conform)
    conform.formatters = conform.formatters or {}
    conform.formatters.csharpier = {
      command = vim.fn.executable("csharpier") == 1 and "csharpier" or "dotnet-csharpier",
      args = { "--write-stdout" },
    }
  end,
  neotest = { ["neotest-dotnet"] = function() return require("neotest-dotnet") end },
  plugins = {
    {
      "Hoffs/omnisharp-extended-lsp.nvim",
      ft = { "cs", "csproj", "razor" },
    },
    {
      "Issafalcon/neotest-dotnet",
      ft = "cs",
      dependencies = { "neotest" },
    },
  },
})

if vim.fn.executable("dotnet") == 1 then
  -- DAP wiring lives outside the helper since dap configurations don't fit
  -- the helper's shape. Cmd-gated so it mirrors the helper's gate.
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
end

return plugins
