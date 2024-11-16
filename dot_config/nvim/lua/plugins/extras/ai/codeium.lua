return {
  {
    "Exafunction/codeium.nvim",
    cmd = "Codeium",
    build = ":Codeium Auth",
    opts = {
      enable_cmp_source = true,
      virtual_text = {
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

  {
    "nvim-cmp",
    optional = true,
    dependencies = { "codeium.nvim" },
    opts = function(_, opts)
      table.insert(opts.sources, 1, {
        name = "codeium",
        group_index = 1,
        priority = 100,
      })
    end,
  },

  {
    "saghen/blink.cmp",
    optional = true,
    opts = {
      sources = {
        compat = { "codeium" },
      },
    },
    dependencies = {
      "codeium.nvim",
      "saghen/blink.compat",
    },
  },
}
