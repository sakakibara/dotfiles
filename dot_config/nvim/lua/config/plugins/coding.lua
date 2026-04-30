-- lua/config/plugins/coding.lua
return {
  {
    "saghen/blink.cmp",
    name = "blink.cmp",
    version = "1.*",  -- uses released prebuilt Rust binary
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = { "friendly-snippets", "lazydev.nvim" },
    config = function(_, opts)
      -- blink.cmp's utils.notify uses vim.api.nvim_echo({...}, true, ...)
      -- (history=true). With our cmdheight=0, those echoes have no
      -- cmdline area, so nvim displays them as a brief stacked popup at
      -- the top of the screen — the "Downloading pre-built binary" /
      -- "Downloaded..." flashes the user sees on first InsertEnter.
      -- The same text also flows through ext_messages → noice → snacks
      -- toast, producing a double display. Monkey-patch the notify
      -- function to route via vim.notify instead, which goes through
      -- snacks cleanly with no cmdline echo.
      local utils = require("blink.cmp.lib.utils")
      utils.notify = function(msg, lvl)
        local out = {}
        for _, chunk in ipairs(msg or {}) do
          out[#out + 1] = chunk[1] or ""
        end
        vim.notify(table.concat(out), lvl)
      end
      require("blink.cmp").setup(opts)
    end,
    opts = {
      keymap = { preset = "default" },
      appearance = { nerd_font_variant = "mono" },
      completion = {
        accept = { auto_brackets = { enabled = true } },
        documentation = { auto_show = true, auto_show_delay_ms = 200 },
        ghost_text = { enabled = false },
      },
      sources = {
        default = { "lsp", "path", "snippets", "buffer", "lazydev", "chezmoi" },
        providers = {
          lazydev = {
            name = "LazyDev",
            module = "lazydev.integrations.blink",
            score_offset = 100,
          },
          chezmoi = {
            name = "Chezmoi",
            module = "lib.chezmoi.blink",
            score_offset = 50,
          },
        },
      },
      fuzzy = { implementation = "prefer_rust" },
      snippets = { preset = "default" },
    },
  },

  -- Community snippet collection (vscode-style snippets)
  { "rafamadriz/friendly-snippets", name = "friendly-snippets", lazy = true },

  -- Icon provider: alternative to nvim-web-devicons. Kept available for
  -- plugins that prefer it; web-devicons remains the primary.
  { "echasnovski/mini.icons", name = "mini.icons", lazy = true, opts = {} },
}
