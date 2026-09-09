vim.filetype.add({
  pattern = {
    [".*/playbooks?/.*%.ya?ml"] = "yaml.ansible",
    [".*/roles/.*/tasks/.*%.ya?ml"] = "yaml.ansible",
    [".*/roles/.*/handlers/.*%.ya?ml"] = "yaml.ansible",
    [".*/group_vars/.*%.ya?ml"] = "yaml.ansible",
    [".*/host_vars/.*%.ya?ml"] = "yaml.ansible",
    [".*/ansible/.*%.ya?ml"] = "yaml.ansible",
    [".*/playbook[^/]*%.ya?ml"] = "yaml.ansible",
    [".*[-_.]playbook[^/]*%.ya?ml"] = "yaml.ansible",
  },
})

return Lib.lang.setup({
  cmd = "ansible",
  ft = "yaml.ansible",
  parsers = { "yaml" },
  mason = { "ansible-language-server", "ansible-lint" },
  servers = { ansiblels = {} },
  linters = { ["yaml.ansible"] = { "ansible_lint" } },
  plugins = {
    {
      "mfussenegger/nvim-ansible",
      ft = {},
      keys = {
        {
          "<Leader>ta",
          function() require("ansible").run() end,
          desc = "Ansible run playbook/role",
          silent = true,
        },
      },
    },
  },
})
