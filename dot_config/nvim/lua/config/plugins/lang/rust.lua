-- lua/config/plugins/lang/rust.lua
if vim.fn.executable("rustc") == 0 then return {} end

-- Per user policy, rust-analyzer is NOT auto-installed. Install via:
--   rustup component add rust-analyzer

-- codelldb is shared with c.lua; Lib.mason.add is idempotent.
Lib.mason.add("codelldb")

Lib.neotest.add("rustaceanvim", function() return require("rustaceanvim.neotest") end)

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "rust", "ron" })
end)

-- NOTE: no vim.lsp.config for rust_analyzer — rustaceanvim owns it entirely.
-- NOTE: bacon_ls fallback (old config gated it on `diagnostics == "bacon-ls"`)
-- dropped — user has no global `diagnostics` variable in the ported config.

return {
  {
    "mrcjkb/rustaceanvim",
    name = "rustaceanvim",
    ft = { "rust" },
    init = function()
      -- Resolve codelldb adapter via rustaceanvim's built-in helper. Platform
      -- detection picks the correct liblldb library. Falls back to the plain
      -- codelldb executable if the mason path or helper is unavailable.
      local function codelldb_adapter()
        local ok_cfg, rustacean_cfg = pcall(require, "rustaceanvim.config")
        if not ok_cfg then return nil end
        local package_path = vim.fn.stdpath("data") .. "/mason/packages/codelldb"
        local codelldb = package_path .. "/extension/adapter/codelldb"
        local library_path = package_path .. "/extension/lldb/lib/liblldb.dylib"
        if vim.fn.has("linux") == 1 then
          library_path = package_path .. "/extension/lldb/lib/liblldb.so"
        end
        return rustacean_cfg.get_codelldb_adapter(codelldb, library_path)
      end

      vim.g.rustaceanvim = {
        server = {
          capabilities = Lib.lsp.capabilities(),
          default_settings = {
            ["rust-analyzer"] = {
              cargo = {
                allFeatures = true,
                loadOutDirsFromCheck = true,
                buildScripts = {
                  enable = true,
                },
              },
              -- rust-analyzer handles diagnostics itself (no bacon-ls switch).
              checkOnSave = true,
              diagnostics = {
                enable = true,
              },
              procMacro = {
                enable = true,
                ignored = {
                  ["async-trait"] = { "async_trait" },
                  ["napi-derive"] = { "napi" },
                  ["async-recursion"] = { "async_recursion" },
                },
              },
              files = {
                excludeDirs = {
                  ".direnv",
                  ".git",
                  ".github",
                  ".gitlab",
                  "bin",
                  "node_modules",
                  "target",
                  "venv",
                  ".venv",
                },
              },
            },
          },
        },
        dap = {
          adapter = codelldb_adapter(),
        },
        -- NOTE: `<Leader>cR` RustLsp codeAction keymap dropped — framework
        -- uses stock LSP keymaps.
      }
    end,
  },

  {
    "saecki/crates.nvim",
    name = "crates.nvim",
    -- Load on any TOML file; crates.nvim internally activates only for
    -- Cargo.toml buffers. The pack framework's event trigger doesn't
    -- support pattern-qualified events like "BufRead Cargo.toml".
    ft = { "toml" },
    opts = {
      completion = {
        crates = {
          enabled = true,
        },
      },
      lsp = {
        enabled = true,
        actions = true,
        completion = true,
        hover = true,
      },
    },
  },

}
