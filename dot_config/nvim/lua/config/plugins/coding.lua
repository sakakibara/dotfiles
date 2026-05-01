-- lua/config/plugins/coding.lua

-- Replace blink.cmp.lib.utils.notify with a splash-aware version. The
-- stock notify uses vim.api.nvim_echo with history=true; with our
-- cmdheight=0 that produces a brief "more"-style popup at the top of
-- the screen plus a duplicated toast. Our replacement:
--   - During cold-install splash → splash status_text (centered box)
--   - Otherwise → vim.notify (route via the noice mini route to
--     bottom-right strip; no top-of-screen flash)
local function patch_utils_notify(utils)
  utils.notify = function(msg, lvl)
    local out = {}
    for _, chunk in ipairs(msg or {}) do
      out[#out + 1] = chunk[1] or ""
    end
    local text = table.concat(out)
    local ok_ui, UI = pcall(require, "core.pack.ui")
    if ok_ui and UI._active_splash then
      UI._active_splash:set_status_text(text)
    else
      vim.notify(text, lvl)
    end
  end
end

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
          local ok_utils, utils = pcall(require, "blink.cmp.lib.utils")
          if ok_utils then patch_utils_notify(utils) end
          pcall(function()
            require("blink.cmp.fuzzy.download").ensure_downloaded(function() end)
          end)
        end,
      })
    end,
    config = function(_, opts)
      -- Re-apply the utils.notify patch in case blink loads via its
      -- `event` trigger before our init's VeryLazy autocmd has fired.
      patch_utils_notify(require("blink.cmp.lib.utils"))
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
