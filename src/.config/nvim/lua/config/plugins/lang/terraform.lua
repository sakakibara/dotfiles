-- No terraform pickers: the telescope extensions that provide them have no
-- counterpart for snacks.picker.
vim.treesitter.language.register("terraform", { "terraform-vars" })

return Lib.lang.setup({
  cmd = "terraform",
  ft = { "terraform", "terraform-vars", "hcl" },
  mason = { "terraform-ls" },
  parsers = { "terraform", "hcl" },
  servers = { terraformls = {} },
  formatters = {
    terraform = { "terraform_fmt" },
    ["terraform-vars"] = { "terraform_fmt" },
  },
  linters = {
    terraform = { "terraform_validate" },
  },
})
