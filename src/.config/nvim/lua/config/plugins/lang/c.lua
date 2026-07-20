-- DAP adapter + per-language configurations. Registered unconditionally;
-- adapters/configurations only fire when the user starts a debug session.
Lib.plugin.on_load("nvim-dap", function()
  local dap = require("dap")
  if not dap.adapters["codelldb"] then
    dap.adapters["codelldb"] = {
      type = "server",
      host = "localhost",
      port = "${port}",
      executable = {
        command = "codelldb",
        args = { "--port", "${port}" },
      },
    }
  end
  for _, lang in ipairs({ "c", "cpp" }) do
    dap.configurations[lang] = {
      {
        type = "codelldb",
        request = "launch",
        name = "Launch file",
        program = function()
          return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
        end,
        cwd = "${workspaceFolder}",
      },
      {
        type = "codelldb",
        request = "attach",
        name = "Attach to process",
        pid = require("dap.utils").pick_process,
        cwd = "${workspaceFolder}",
      },
    }
  end
end)

-- Header/source switch keymap (C/C++ buffers). The keymap calls clangd's
-- LSP command; if clangd isn't running, the command fails harmlessly.
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "c", "cpp", "h", "hpp" },
  callback = function(args)
    vim.keymap.set(
      "n",
      "<Leader>ch",
      "<Cmd>LspClangdSwitchSourceHeader<CR>",
      { buffer = args.buf, desc = "Switch source/header (C/C++)" }
    )
  end,
})

return Lib.lang.setup({
  ft = { "c", "cpp" },
  cmd = "gcc",
  mason = { "clangd", "codelldb" },
  parsers = { "c", "cpp" },
  servers = {
    clangd = {
      capabilities = { offsetEncoding = { "utf-16" } },
      cmd = {
        "clangd",
        "--background-index",
        "--clang-tidy",
        "--header-insertion=iwyu",
        "--completion-style=detailed",
        "--function-arg-placeholders",
        "--fallback-style=llvm",
      },
      init_options = {
        usePlaceholders = true,
        completeUnimported = true,
        clangdFileStatus = true,
      },
      root_dir = function(bufnr, on_dir)
        local fname = vim.api.nvim_buf_get_name(bufnr)
        local root = vim.fs.root(fname, {
          "Makefile",
          "configure.ac",
          "configure.in",
          "config.h.in",
          "meson.build",
          "meson_options.txt",
          "build.ninja",
        }) or vim.fs.root(fname, { "compile_commands.json", "compile_flags.txt" }) or vim.fs.root(fname, { ".git" })
        if root then on_dir(root) end
      end,
    },
  },
  plugins = {
    {
      "p00f/clangd_extensions.nvim",
      ft = { "c", "cpp", "h", "hpp" },
      opts = {
        inlay_hints = {
          inline = false,
        },
        ast = {
          role_icons = {
            type = "",
            declaration = "",
            expression = "",
            specifier = "",
            statement = "",
            ["template argument"] = "",
          },
          kind_icons = {
            Compound = "",
            Recovery = "",
            TranslationUnit = "",
            PackExpansion = "",
            TemplateTypeParm = "",
            TemplateTemplateParm = "",
            TemplateParamObject = "",
          },
        },
      },
    },
  },
})
