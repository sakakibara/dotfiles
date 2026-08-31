return Lib.lang.setup({
  cmd = "pdflatex",
  ft = { "tex", "plaintex", "bib" },
  mason = { "texlab" },
  parsers = { "latex", "bibtex" },
  servers = { texlab = {} },
  plugins = {
    {
      "lervag/vimtex",
      -- vimtex ships ftplugin/ files and needs to load before the first tex
      -- buffer to register filetype detection/autocommands.
      lazy = false,
      init = function()
        vim.g.vimtex_mappings_disable = { ["n"] = { "K" } }
        vim.g.vimtex_quickfix_method = vim.fn.executable("pplatex") == 1 and "pplatex" or "latexlog"
      end,
      keys = {
        { "<LocalLeader>l", "", desc = "+vimtex" },
      },
    },
  },
})
