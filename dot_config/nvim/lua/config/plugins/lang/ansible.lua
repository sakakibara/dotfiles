-- lua/config/plugins/lang/ansible.lua
return Lib.lang.setup({
  cmd = "ansible",
  mason = { "ansible-language-server", "ansible-lint" },
  servers = { ansiblels = {} },
  linters = { ["yaml.ansible"] = { "ansible_lint" } },
  plugins = {
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
  },
})
