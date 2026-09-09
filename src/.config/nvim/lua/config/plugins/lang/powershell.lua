-- powershell-editor-services is a bundled DLL set launched via pwsh; mason
-- installs it under its packages dir, where `bundle_path` points.
vim.treesitter.language.register("powershell", { "ps1" })

local bundle_path = vim.fn.stdpath("data") .. "/mason/packages/powershell-editor-services"

return Lib.lang.setup({
  cmd = "pwsh",
  ft = "ps1",
  parsers = { "powershell" },
  mason = { "powershell-editor-services" },
  servers = {
    powershell_es = {
      available = function() return vim.fn.isdirectory(bundle_path) == 1 end,
      bundle_path = bundle_path,
    },
  },
  plugins = {
    { "sakakibara/vim-ps1", ft = "ps1" },
  },
})
