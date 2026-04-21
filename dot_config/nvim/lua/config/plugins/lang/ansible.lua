-- lua/config/plugins/lang/ansible.lua
if vim.fn.executable("ansible") == 0 then return {} end

Lib.mason.add("ansible-language-server", "ansible-lint")

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("ansiblels", { capabilities = Lib.lsp.capabilities() })
  Lib.lsp.enable("ansiblels")
end)

Lib.plugin.on_load("nvim-lint", function()
  require("lint").linters_by_ft["yaml.ansible"] = { "ansible_lint" }
end)

return {
  {
    "mfussenegger/nvim-ansible",
    name = "nvim-ansible",
    ft = {},
    keys = {
      {
        "<Leader>ta",
        function() require("ansible").run() end,
        desc = "Ansible Run Playbook/Role",
        silent = true,
      },
    },
  },
}
