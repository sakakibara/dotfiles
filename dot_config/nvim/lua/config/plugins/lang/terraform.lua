-- lua/config/plugins/lang/terraform.lua
if vim.fn.executable("terraform") == 0 then return {} end

Lib.mason.add("terraform-ls")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "terraform", "hcl" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("terraformls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("terraformls")
end)

Lib.plugin.on_load("conform.nvim", function()
  local conform = require("conform")
  conform.formatters_by_ft.terraform = { "terraform_fmt" }
  conform.formatters_by_ft.tf = { "terraform_fmt" }
  conform.formatters_by_ft["terraform-vars"] = { "terraform_fmt" }
end)

Lib.plugin.on_load("nvim-lint", function()
  local lint = require("lint")
  lint.linters_by_ft.terraform = { "terraform_validate" }
  lint.linters_by_ft.tf = { "terraform_validate" }
end)

-- Note: old config registered `telescope-terraform.nvim` and
-- `telescope-terraform-doc.nvim` extensions. Skipped — M2 has no telescope.
-- Revisit if/when telescope lands.

return {}
