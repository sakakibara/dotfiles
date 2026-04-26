-- lua/config/plugins/treesitter.lua
--
-- nvim-treesitter main branch (active, Nvim 0.12+). master is the locked
-- legacy branch for Nvim 0.11 — it has the iter_matches API mismatch.
-- main dropped the configs module; highlighting/folds/indent are wired
-- per-filetype via native vim.treesitter APIs.

return {
  {
    "nvim-treesitter/nvim-treesitter",
    name = "nvim-treesitter",
    branch = "main",
    lazy = false,  -- main explicitly does NOT support lazy loading
    build = ":TSUpdate",
    init = function()
      -- main branch compiles parsers on-demand and requires tree-sitter-cli
      -- (>= 0.26.1) on PATH. mason's registry has it.
      Lib.mason.add("tree-sitter-cli")
    end,
    config = function()
      require("nvim-treesitter").setup({
        install_dir = vim.fn.stdpath("data") .. "/site",
      })

      -- Parsers to ensure installed (asynchronous; no-op if already present).
      require("nvim-treesitter").install({
        "bash", "c", "cpp", "css", "html", "javascript", "json",
        "lua", "luadoc", "markdown", "markdown_inline", "python",
        "query", "regex", "rust", "styled", "toml", "tsx", "typescript",
        "vim", "vimdoc", "yaml",
      })

      -- Highlight on every filetype where a parser exists.
      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("Lib.treesitter.highlight", { clear = true }),
        callback = function(args)
          local bufnr = args.buf
          -- snacks.bigfile retags huge buffers as ft=bigfile and sets
          -- foldmethod=manual etc. Without this gate we'd override that
          -- and re-arm vim.treesitter.foldexpr() on a parser-less buffer,
          -- which evaluates on every fold event and freezes the editor
          -- on multi-MB files (panic logs, minified bundles).
          if vim.bo[bufnr].filetype == "bigfile" then return end
          -- vim.treesitter.start handles its own parser availability check
          pcall(vim.treesitter.start, bufnr)
          -- Enable tree-sitter folds
          pcall(function()
            vim.wo[0][0].foldexpr   = "v:lua.vim.treesitter.foldexpr()"
            vim.wo[0][0].foldmethod = "expr"
          end)
          -- Enable tree-sitter indent (experimental per docs)
          pcall(function()
            vim.bo[bufnr].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
          end)
        end,
      })
    end,
  },

  -- Textobjects — has a main branch that pairs with nvim-treesitter main
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    name = "nvim-treesitter-textobjects",
    branch = "main",
    lazy = false,
    dependencies = { "nvim-treesitter" },
    -- Note: af/if/ac/ic/aa/ia are provided by mini.ai (see editor.lua)
    -- via treesitter gen_spec; only jump motions live here.
    keys = {
      { "]f", function() require("nvim-treesitter-textobjects.move").goto_next_start("@function.outer", "textobjects") end,     desc = "Next function start" },
      { "[f", function() require("nvim-treesitter-textobjects.move").goto_previous_start("@function.outer", "textobjects") end, desc = "Prev function start" },
      { "]c", function() require("nvim-treesitter-textobjects.move").goto_next_start("@class.outer", "textobjects") end,        desc = "Next class start" },
      { "[c", function() require("nvim-treesitter-textobjects.move").goto_previous_start("@class.outer", "textobjects") end,    desc = "Prev class start" },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    name = "nvim-treesitter-context",
    event = "LazyFile",
    opts = {
      max_lines = 3,
      trim_scope = "outer",
      mode = "cursor",
    },
  },

  {
    "windwp/nvim-ts-autotag",
    name = "nvim-ts-autotag",
    ft = { "html", "xml", "jsx", "tsx", "vue", "svelte", "astro", "markdown" },
    opts = {},
  },
}
