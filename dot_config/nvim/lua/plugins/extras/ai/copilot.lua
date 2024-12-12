return {
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    build = ":Copilot auth",
    event = "InsertEnter",
    opts = {
      suggestion = {
        enabled = false,
        auto_trigger = true,
        keymap = {
          accept = false,
          next = "<M-]>",
          prev = "<M-[>",
        },
      },
      panel = { enabled = false },
      filetypes = {
        markdown = true,
        help = true,
      },
    },
  },

  {
    "zbirenbaum/copilot.lua",
    opts = function()
      Util.cmp.actions.ai_accept = function()
        if require("copilot.suggestion").is_visible() then
          Util.keymaps.create_undo()
          require("copilot.suggestion").accept()
          return true
        end
      end
    end,
  },

  {
    "nvim-cmp",
    optional = true,
    dependencies = {
      {
        "zbirenbaum/copilot-cmp",
        opts = {},
        config = function(_, opts)
          local copilot_cmp = require("copilot_cmp")
          copilot_cmp.setup(opts)
          Util.lsp.on_attach(function()
            copilot_cmp._on_insert_enter({})
          end, "copilot")
        end,
        specs = {
          {
            "nvim-cmp",
            optional = true,
            opts = function(_, opts)
              table.insert(opts.sources, 1, {
                name = "copilot",
                group_index = 1,
                priority = 100,
              })
            end,
          },
        },
      },
    },
  },

  {
    "saghen/blink.cmp",
    optional = true,
    dependencies = {
      {
        "giuxtaposition/blink-cmp-copilot",
        specs = {
          {
            "blink.cmp",
            optional = true,
            opts = {
              sources = {
                default = { "copilot" },
                providers = {
                  copilot = {
                    name = "copilot",
                    module = "blink-cmp-copilot",
                    kind = "Copilot",
                  },
                },
              },
            },
          },
        },
      },
    },
  },
}
