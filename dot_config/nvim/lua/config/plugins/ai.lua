-- lua/config/plugins/ai.lua
return {
  {
    "yetone/avante.nvim",
    name = "avante.nvim",
    event = "VeryLazy",
    build = "make",
    dependencies = {
      "nvim-treesitter",
      "nui.nvim",
      "nvim-web-devicons",
      "plenary.nvim",
    },
    keys = {
      { "<leader>aa", "<cmd>AvanteAsk<cr>",     desc = "Avante ask",    mode = { "n", "x" } },
      { "<leader>ae", "<cmd>AvanteEdit<cr>",    desc = "Avante edit",   mode = { "n", "x" } },
      { "<leader>ar", "<cmd>AvanteRefresh<cr>", desc = "Avante refresh" },
      { "<leader>at", "<cmd>AvanteToggle<cr>",  desc = "Avante toggle" },
      { "<leader>an", "<cmd>AvanteNew<cr>",     desc = "Avante new chat" },
    },
    opts = {
      provider = "claude",
      providers = {
        claude = {
          endpoint = "https://api.anthropic.com",
          model = "claude-sonnet-4-6",
          extra_request_body = {
            temperature = 0,
            max_tokens = 8192,
          },
        },
      },
      behaviour = {
        auto_suggestions = false,
        auto_set_highlight_group = true,
        auto_set_keymaps = false,
      },
    },
  },
}
