-- lua/config/plugins/lang/tex.lua
if vim.fn.executable("pdflatex") == 0 then return {} end

Lib.mason.add("texlab")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "latex", "bibtex" })
end)

-- latex treesitter highlight conflicts with vimtex's syntax; disable it.
Lib.plugin.on_load("nvim-treesitter", function()
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
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("texlab", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("texlab")
end)

return {
  {
    "lervag/vimtex",
    name = "vimtex",
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
}
