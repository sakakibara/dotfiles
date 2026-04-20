-- lua/config/plugins/lang/haskell.lua
if vim.fn.executable("ghc") == 0 then return {} end

-- haskell-language-server: install via ghcup (`ghcup install hls`); not managed by mason.

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "haskell" })
end)

-- haskell-tools.nvim configures the LSP on its own via vim.g.haskell_tools;
-- no nvim-lspconfig wiring needed.

Lib.neotest.add("neotest-haskell", function() return require("neotest-haskell") end)

return {
  {
    "mrcjkb/haskell-tools.nvim",
    name = "haskell-tools.nvim",
    version = "^3",
    ft = { "haskell", "lhaskell", "cabal", "cabalproject" },
    -- NOTE: `telescope_hoogle` extension skipped (telescope not in M2 framework).
    init = function()
      vim.g.haskell_tools = {
        hls = {
          capabilities = Lib.lsp.capabilities(),
        },
        dap = {
          cmd = { "haskell-debug-adapter" },
        },
      }
    end,
  },

  {
    "mrcjkb/neotest-haskell",
    name = "neotest-haskell",
    ft = "haskell",
    dependencies = { "neotest" },
  },

  {
    "mrcjkb/haskell-snippets.nvim",
    name = "haskell-snippets.nvim",
    ft = { "haskell", "lhaskell", "cabal", "cabalproject" },
    -- Requires LuaSnip at runtime. If LuaSnip isn't installed, we skip silently.
    config = function()
      local ok_ls, luasnip = pcall(require, "luasnip")
      if not ok_ls then return end
      local haskell_snippets = require("haskell-snippets").all
      luasnip.add_snippets("haskell", haskell_snippets, { key = "haskell" })
    end,
  },
}
