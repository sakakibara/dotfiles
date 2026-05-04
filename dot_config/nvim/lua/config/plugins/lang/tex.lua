-- lua/config/plugins/lang/tex.lua
return Lib.lang.setup({
  cmd = "pdflatex",
  mason = { "texlab" },
  parsers = { "latex", "bibtex" },
  parsers_setup = function()
    -- latex treesitter highlight conflicts with vimtex's syntax; disable it.
    local ok, configs = pcall(require, "nvim-treesitter.configs")
    if ok and configs.get_module then
      local hl = configs.get_module("highlight")
      if hl then
        hl.disable = hl.disable or {}
        if type(hl.disable) == "table" then
          table.insert(hl.disable, "latex")
        end
      end
    end
  end,
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
