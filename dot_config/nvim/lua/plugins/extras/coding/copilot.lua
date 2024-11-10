return {
  {
    "zbirenbaum/copilot.lua",
    event = "BufEnter",
    cmd = "Copilot",
    build = ":Copilot auth",
    opts = {
      suggestion = { auto_trigger = true },
      panel = { enabled = false },
      filetypes = {
        markdown = true,
        help = true,
      },
    },
  },

  {
    "CopilotC-Nvim/CopilotChat.nvim",
    branch = "canary",
    dependencies = {
      { "zbirenbaum/copilot.lua" },
      { "nvim-lua/plenary.nvim" },
    },
    build = "make tiktoken",
    cmd = {
      "CopilotChat",
      "CopilotChatOpen",
      "CopilotChatClose",
      "CopilotChatToggle",
      "CopilotChatStop",
      "CopilotChatReset",
      "CopilotChatSave",
      "CopilotChatLoad",
      "CopilotChatDebugInfo",
      "CopilotChatModels",
      "CopilotChatModel",
    },
    keys = {
      { "<Leader>c;", ":CopilotChatToggle<CR>", desc = "Copilot chat" },
    },
    opts = {
      mappings = {
        accept_diff = {
          normal = "g<C-y>",
          insert = "g<C-y>",
        },
      },
    },
  },
}
