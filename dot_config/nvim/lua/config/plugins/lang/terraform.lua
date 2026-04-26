-- lua/config/plugins/lang/terraform.lua
-- Note: old config registered `telescope-terraform.nvim` and
-- `telescope-terraform-doc.nvim` extensions. Skipped — M2 has no telescope.
-- Revisit if/when telescope lands.
return Lib.lang.setup({
  cmd = "terraform",
  mason = { "terraform-ls" },
  parsers = { "terraform", "hcl" },
  servers = { terraformls = {} },
  formatters = {
    terraform = { "terraform_fmt" },
    tf = { "terraform_fmt" },
    ["terraform-vars"] = { "terraform_fmt" },
  },
  linters = {
    terraform = { "terraform_validate" },
    tf = { "terraform_validate" },
  },
})
