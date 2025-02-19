return {
  {
    "williamboman/mason.nvim",
    opts = { ensure_installed = { "ansible-lint" } },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        ansiblels = {},
      },
    },
  },

  {
    "mfussenegger/nvim-ansible",
    ft = {},
    keys = {
      {
        "<Leader>ta",
        function()
          require("ansible").run()
        end,
        desc = "Ansible Run Playbook/Role",
        silent = true,
      },
    },
  },
}
