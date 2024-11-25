return {
  {
    "hrsh7th/nvim-cmp",
    enabled = false,
  },
  {
    "saghen/blink.cmp",
    version = "*",
    opts_extend = {
      "sources.completion.enabled_providers",
      "sources.compat",
    },
    dependencies = {
      "rafamadriz/friendly-snippets",
    },
    event = "InsertEnter",

    opts = {
      highlight = {
        use_nvim_cmp_as_default = false,
      },
      nerd_font_variant = "mono",
      windows = {
        autocomplete = {
          winblend = vim.o.pumblend,
        },
        documentation = {
          auto_show = true,
        },
        ghost_text = {
          enabled = true,
        },
      },

      accept = { auto_brackets = { enabled = true } },

      sources = {
        compat = {},
        completion = {
          enabled_providers = { "lsp", "path", "snippets", "buffer" },
        },
      },

      keymap = {
        preset = "enter",
        ["<Tab>"] = {
          Util.cmp.map({ "snippet_forward", "ai_accept" }),
          "fallback",
        },
      },
    },
    config = function(_, opts)
      local enabled = opts.sources.completion.enabled_providers
      for _, source in ipairs(opts.sources.compat or {}) do
        opts.sources.providers[source] = vim.tbl_deep_extend(
          "force",
          { name = source, module = "blink.compat.source" },
          opts.sources.providers[source] or {}
        )
        if type(enabled) == "table" and not vim.tbl_contains(enabled, source) then
          table.insert(enabled, source)
        end
      end
      require("blink.cmp").setup(opts)
    end,
  },

  {
    "saghen/blink.cmp",
    opts = {
      sources = {
        completion = {
          enabled_providers = { "lazydev" },
        },
        providers = {
          lsp = {
            fallback_for = { "lazydev" },
          },
          lazydev = {
            name = "LazyDev",
            module = "lazydev.integrations.blink",
          },
        },
      },
    },
  },
}
