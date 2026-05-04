-- lua/config/plugins/ai.lua
return {
  {
    "yetone/avante.nvim",
    event = "VeryLazy",
    build = "make",
    dependencies = {
      "nvim-treesitter",
      "nui.nvim",
      "nvim-web-devicons",
      "plenary.nvim",
    },
    keys = {
      { "<Leader>aa", "<Cmd>AvanteAsk<CR>",     desc = "Avante ask",    mode = { "n", "x" } },
      { "<Leader>ae", "<Cmd>AvanteEdit<CR>",    desc = "Avante edit",   mode = { "n", "x" } },
      { "<Leader>ar", "<Cmd>AvanteRefresh<CR>", desc = "Avante refresh" },
      { "<Leader>at", "<Cmd>AvanteToggle<CR>",  desc = "Avante toggle" },
      { "<Leader>an", "<Cmd>AvanteNew<CR>",     desc = "Avante new chat" },
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
