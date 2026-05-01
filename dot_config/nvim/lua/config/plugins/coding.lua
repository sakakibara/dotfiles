-- lua/config/plugins/coding.lua
return {
  {
    "saghen/blink.cmp",
    name = "blink.cmp",
    version = "1.*",  -- uses released prebuilt Rust binary
    event = { "InsertEnter", "CmdlineEnter" },
    dependencies = { "friendly-snippets", "lazydev.nvim" },
    init = function()
      -- Pre-download blink's Rust binary during the cold-install splash
      -- window so it's ready by the time the user starts typing. Only
      -- fires when the cold-install splash is actually up — i.e., this
      -- is a first-run / fresh-state-dir startup. On every subsequent
      -- startup the binary is already on disk and blink's lazy load
      -- handles the no-op `ensure_downloaded` itself; we don't even
      -- packadd here. Gated on UI._active_splash so the work is
      -- exclusive to the first-run scenario the user wants it for.
      vim.api.nvim_create_autocmd("User", {
        pattern  = "VeryLazy",
        once     = true,
        callback = function()
          local ok_ui, UI = pcall(require, "core.pack.ui")
          if not (ok_ui and UI._active_splash) then return end

          pcall(vim.cmd, "packadd blink.cmp")
          -- Apply the utils.notify monkey-patch BEFORE triggering the
          -- download so the download's notifications route via
          -- vim.notify (and our noice mini-view route) rather than
          -- nvim_echo. See the same patch in config() for the
          -- explanatory comment.
          local ok_utils, utils = pcall(require, "blink.cmp.lib.utils")
          if ok_utils then
            utils.notify = function(msg, lvl)
              local out = {}
              for _, chunk in ipairs(msg or {}) do
                out[#out + 1] = chunk[1] or ""
              end
              vim.notify(table.concat(out), lvl)
            end
          end
          pcall(function()
            require("blink.cmp.fuzzy.download").ensure_downloaded(function() end)
          end)
        end,
      })
    end,
    config = function(_, opts)
      -- Re-apply the utils.notify monkey-patch in case blink loads
      -- via its `event` trigger before our init's VeryLazy autocmd
      -- has fired (e.g., if user types `i` before VeryLazy).
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
