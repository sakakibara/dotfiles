-- lua/config/plugins/lang/clojure.lua
if vim.fn.executable("clj") == 0 then return {} end

Lib.mason.add("clojure-lsp")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "clojure" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("clojure_lsp", { capabilities = Lib.lsp.capabilities() })
  Lib.lsp.enable("clojure_lsp")
end)

return {
  {
    "m00qek/baleia.nvim",
    name = "baleia.nvim",
    lazy = true,
    config = function()
      local opts = { line_starts_at = 3 }
      vim.g.conjure_baleia = require("baleia").setup(opts)

      vim.api.nvim_create_user_command("BaleiaColorize", function()
        vim.g.conjure_baleia.once(vim.api.nvim_get_current_buf())
      end, { bang = true })

      vim.api.nvim_create_user_command("BaleiaLogs", vim.g.conjure_baleia.logger.show, { bang = true })
    end,
  },

  {
    "Olical/conjure",
    name = "conjure",
    ft = { "clojure", "fennel", "scheme", "janet", "lisp", "racket" },
    config = function()
      require("conjure.main").main()
      require("conjure.mapping")["on-filetype"]()
    end,
    init = function()
      -- baleia is lazy-loaded on demand; trigger it so conjure can colorize.
      local ok_baleia = pcall(function() require("baleia") end)
      if ok_baleia then
        vim.g["conjure#log#strip_ansi_escape_sequences_line_limit"] = 0
      else
        vim.g["conjure#log#strip_ansi_escape_sequences_line_limit"] = 1
      end

      vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
        pattern = "conjure-log-*",
        callback = function()
          local buffer = vim.api.nvim_get_current_buf()
          vim.diagnostic.enable(false, { bufnr = buffer })

          if ok_baleia and vim.g.conjure_baleia then
            vim.g.conjure_baleia.automatically(buffer)
          end

          vim.keymap.set(
            { "n", "v" },
            "[c",
            "<Cmd>call search('^; -\\+$', 'bw')<CR>",
            { silent = true, buffer = true, desc = "Jumps to the beginning of previous evaluation output." }
          )
          vim.keymap.set(
            { "n", "v" },
            "]c",
            "<Cmd>call search('^; -\\+$', 'w')<CR>",
            { silent = true, buffer = true, desc = "Jumps to the beginning of next evaluation output." }
          )
        end,
      })

      vim.g["conjure#mapping#doc_word"] = "K"
      vim.g["conjure#mapping#def_word"] = "gd"
    end,
  },

  {
    "PaterJason/nvim-treesitter-sexp",
    name = "nvim-treesitter-sexp",
    ft = { "clojure", "fennel", "scheme", "janet", "lisp", "racket" },
    opts = {},
  },
}
