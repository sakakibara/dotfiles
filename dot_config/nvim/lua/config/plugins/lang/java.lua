-- lua/config/plugins/lang/java.lua
if vim.fn.executable("java") == 0 then return {} end

Lib.mason.add("jdtls")
Lib.mason.add("java-debug-adapter", "java-test")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "java" })
end)

-- nvim-jdtls owns jdtls startup entirely; no nvim-lspconfig registration.

return {
  {
    "mfussenegger/nvim-jdtls",
    name = "nvim-jdtls",
    ft = "java",
    config = function()
      -- Build DAP bundles from mason paths (java-debug-adapter + java-test).
      local function build_bundles()
        local bundles = {}
        local mason_root = vim.fn.stdpath("data") .. "/mason/packages"
        local jar_patterns = {
          mason_root .. "/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar",
          mason_root .. "/java-test/extension/server/*.jar",
        }
        for _, jar_pattern in ipairs(jar_patterns) do
          for _, bundle in ipairs(vim.split(vim.fn.glob(jar_pattern), "\n")) do
            if bundle ~= "" then table.insert(bundles, bundle) end
          end
        end
        return bundles
      end

      local function attach_jdtls(args)
        local jdtls_cmd = vim.fn.exepath("jdtls")
        if jdtls_cmd == "" then return end

        local fname = vim.api.nvim_buf_get_name(args.buf)
        local root = vim.fs.root(fname, {
          ".git",
          "mvnw",
          "gradlew",
          "pom.xml",
          "build.gradle",
          "build.gradle.kts",
          "settings.gradle",
          "settings.gradle.kts",
        }) or vim.uv.cwd()
        local project_name = vim.fs.basename(root)
        local cache = vim.fn.stdpath("cache")
        local config_dir = cache .. "/jdtls/" .. project_name .. "/config"
        local workspace_dir = cache .. "/jdtls/" .. project_name .. "/workspace"

        local cmd = { jdtls_cmd }
        if project_name then
          vim.list_extend(cmd, {
            "-configuration",
            config_dir,
            "-data",
            workspace_dir,
          })
        end

        require("jdtls").start_or_attach({
          cmd = cmd,
          root_dir = root,
          capabilities = Lib.lsp.capabilities(),
          init_options = {
            bundles = build_bundles(),
          },
          settings = {
            java = {
              inlayHints = {
                parameterNames = {
                  enabled = "all",
                },
              },
            },
          },
        })
      end

      vim.api.nvim_create_autocmd("FileType", {
        pattern = "java",
        callback = attach_jdtls,
      })

      -- Keymaps + DAP setup wired on LspAttach for jdtls buffers.
      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          if not client or client.name ~= "jdtls" then return end
          local jdtls = require("jdtls")
          local map = function(mode, lhs, rhs, desc)
            vim.keymap.set(mode, lhs, rhs, { buffer = args.buf, desc = desc })
          end
          map("n", "<Leader>co", jdtls.organize_imports, "Organize Imports")
          map("n", "<Leader>cxv", jdtls.extract_variable_all, "Extract Variable")
          map("n", "<Leader>cxc", jdtls.extract_constant, "Extract Constant")
          map("v", "<Leader>cxm", [[<Esc><Cmd>lua require('jdtls').extract_method(true)<CR>]], "Extract Method")
          map("v", "<Leader>cxv", [[<Esc><Cmd>lua require('jdtls').extract_variable_all(true)<CR>]], "Extract Variable")
          map("v", "<Leader>cxc", [[<Esc><Cmd>lua require('jdtls').extract_constant(true)<CR>]], "Extract Constant")
          map("n", "<Leader>cgs", jdtls.super_implementation, "Goto Super")

          -- DAP + test wiring. Safe to call repeatedly; jdtls internally guards.
          if Lib.plugin.has("nvim-dap") then
            local ok_dap = pcall(function()
              jdtls.setup_dap({ hotcodereplace = "auto", config_overrides = {} })
              require("jdtls.dap").setup_dap_main_class_configs()
            end)
            if ok_dap then
              map("n", "<Leader>tt", function() require("jdtls.dap").test_class() end, "Run All Test (jdtls)")
              map("n", "<Leader>tr", function() require("jdtls.dap").test_nearest_method() end, "Run Nearest Test (jdtls)")
              map("n", "<Leader>tT", function() require("jdtls.dap").pick_test() end, "Pick Test (jdtls)")
            end
          end
        end,
      })
    end,
  },
}
