-- lua/config/plugins/lang/nix.lua
if vim.fn.executable("nix") == 0 then return {} end

Lib.mason.add("nil")
-- nixfmt not in mason registry; install via `nix profile install nixpkgs#nixfmt-classic` if needed.

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "nix" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("nil_ls", { capabilities = Lib.lsp.capabilities() })
  Lib.lsp.enable("nil_ls")
end)

Lib.plugin.on_load("conform.nvim", function()
  require("conform").formatters_by_ft.nix = { "nixfmt" }
end)

return {}
