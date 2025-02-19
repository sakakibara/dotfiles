return {
  {
    "Exafunction/codeium.nvim",
    cmd = "Codeium",
    event = "InsertEnter",
    build = ":Codeium Auth",
    opts = {
      enable_cmp_source = vim.g.ai_cmp,
      virtual_text = {
        enabled = not vim.g.ai_cmp,
        key_bindings = {
          accept = false,
          next = "<M-]>",
          prev = "<M-[>",
        },
      },
    },
  },

  {
    "Exafunction/codeium.nvim",
    opts = function()
      Util.cmp.actions.ai_accept = function()
        if require("codeium.virtual_text").get_current_completion_item() then
          Util.keymaps.create_undo()
          vim.api.nvim_input(require("codeium.virtual_text").accept())
          return true
        end
      end
    end,
  },

  vim.g.ai_cmp and {
    "saghen/blink.cmp",
    optional = true,
    dependencies = { "codeium.nvim", "saghen/blink.compat" },
    opts = {
      sources = {
        compat = { "codeium" },
        providers = {
          codeium = {
            kind = "Codeium",
            score_offset = 100,
            async = true,
          },
        },
      },
    },
  } or nil,
}
