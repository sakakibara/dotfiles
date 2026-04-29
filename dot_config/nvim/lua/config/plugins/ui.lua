-- lua/config/plugins/ui.lua
return {
  -- web-devicons (used by oil, render-markdown, winbar, tabline)
  -- Eager: statusline, winbar, tabline, oil all call get_icon from first
  -- render. A lazy = true here would make pcall(require, ...) silently
  -- fail before the plugin is packadd-ed and leave icons blank.
  {
    "nvim-tree/nvim-web-devicons",
    name = "nvim-web-devicons",
    lazy = false,
    priority = 950,
  },

  -- noice: cmdline popup + messages routing + LSP hover markdown
  -- Loaded at VeryLazy (matching LazyVim) so core :messages still captures
  -- pre-UI warnings like W325 (swapfile exists). Loading noice any earlier
  -- lets ext_messages swallow those before they hit the message history.
  {
    "folke/noice.nvim",
    name = "noice.nvim",
    event = "VeryLazy",
    dependencies = { "nui.nvim" },
    opts = {
      lsp = {
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"] = true,
        },
        progress = { enabled = false },
        hover = { enabled = true },
        signature = { enabled = true },
      },
      -- Noice runs a 1-second poller that warns if `vim.notify` changed.
      -- We intentionally wrap vim.notify in config/init.lua to tee into
      -- :messages (the wrapper delegates to noice's own `vim.notify` via
      -- `prev`), so the check is a false alarm. Disable it to stop the
      -- periodic warning spam.
      health = { checker = false },
      presets = {
        command_palette = true,
        long_message_to_split = true,
      },
      routes = {
        { filter = { event = "msg_show", any = {
          { find = "%d+L, %d+B" },
          { find = "; after #%d+" },
          { find = "; before #%d+" },
        } }, view = "mini" },
        -- nvim-treesitter "main" branch fires a vim.notify per language
        -- during async install (50+ at once with our parser list). The
        -- long_message_to_split preset routes that burst into a split
        -- window. Skip the noice rendering entirely — :messages still
        -- captures them for diagnostic inspection.
        { filter = { event = "msg_show",
          find = "^%[nvim%-treesitter/install/" }, opts = { skip = true } },
      },
    },
    config = function(_, opts)
      require("noice").setup(opts)
      -- Noice installs a buffer-local K in every markdown-rendered hover
      -- float (noice/text/markdown.lua:244). When the cursor isn't on a
      -- URL/help-tag pattern, its handler calls
      -- `nvim_feedkeys("K", "n", false)` — the "n" flag is noremap, so
      -- our global K (config/init.lua) is bypassed and nvim falls through
      -- to default `keywordprg=:Man`, spawning a subprocess per K press
      -- and producing `man.lua: no manual entry for X` spam on K-repeat.
      -- We delete that buffer-local K immediately after noice installs
      -- it, so our global K wins inside the float. `gx` (URL follow) is
      -- preserved.
      local md = require("noice.text.markdown")
      local orig_keys = md.keys
      md.keys = function(buf)
        orig_keys(buf)
        pcall(vim.keymap.del, "n", "K", { buffer = buf })
      end
    end,
  },

  { "MunifTanjim/nui.nvim", name = "nui.nvim", lazy = true },

  -- Fades the colorcolumn into view only as you approach `textwidth`
  {
    "Bekaboo/deadcolumn.nvim",
    name = "deadcolumn.nvim",
    event = "LazyFile",
    opts = {},
  },

  -- nvim-ufo: modern folding with preview (zp) and LSP/treesitter providers
  {
    "kevinhwang91/nvim-ufo",
    name = "nvim-ufo",
    dependencies = { "promise-async" },
    event = "LazyFile",
    keys = {
      { "zR", function() require("ufo").openAllFolds()                end, desc = "Open all folds" },
      { "zM", function() require("ufo").closeAllFolds()               end, desc = "Close all folds" },
      { "zr", function() require("ufo").openFoldsExceptKinds()        end, desc = "Fold less" },
      { "zm", function() require("ufo").closeFoldsWith()              end, desc = "Fold more" },
      { "zp", function() require("ufo").peekFoldedLinesUnderCursor()  end, desc = "Peek fold" },
    },
    opts = {
      -- ufo's provider_selector accepts at most {main, fallback}. The default
      -- combo is "lsp main, indent universal fallback" — works for any
      -- language with a language server, falls through to indent for the rest.
      -- Per-ft overrides pin a different combo (or single provider) where the
      -- LSP folding ranges are wrong/missing.
      provider_selector = function(_, ft, _)
        local ft_map = {
          markdown = { "treesitter" },
          bash     = { "treesitter", "indent" },
          sh       = { "treesitter", "indent" },
          zsh      = { "treesitter", "indent" },
          css      = { "treesitter", "indent" },
        }
        return ft_map[ft] or { "lsp", "indent" }
      end,
      fold_virt_text_handler = function(virt_text, lnum, end_lnum, width, truncate)
        local new_virt = {}
        local total = vim.api.nvim_buf_line_count(0)
        local folded = end_lnum - lnum
        local suffix = (" %s %d lines (%d%%)"):format(
          Lib.icons.status.Ellipsis, folded, folded / total * 100
        )
        local suf_w = vim.fn.strdisplaywidth(suffix)
        local target = width - suf_w
        local cur = 0
        for _, chunk in ipairs(virt_text) do
          local text, w = chunk[1], vim.fn.strdisplaywidth(chunk[1])
          if target > cur + w then
            table.insert(new_virt, chunk)
          else
            text = truncate(text, target - cur)
            table.insert(new_virt, { text, chunk[2] })
            break
          end
          cur = cur + w
        end
        table.insert(new_virt, { suffix, "MoreMsg" })
        return new_virt
      end,
    },
  },
  { "kevinhwang91/promise-async", name = "promise-async", lazy = true },

  -- indent-blankline (ibl)
  {
    "lukas-reineke/indent-blankline.nvim",
    name = "indent-blankline.nvim",
    main = "ibl",
    event = "LazyFile",
    opts = {
      indent = { char = "│", tab_char = "│" },
      scope = { show_start = false, show_end = false },
      exclude = {
        filetypes = { "help", "alpha", "dashboard", "neo-tree", "lazy", "mason", "notify", "toggleterm", "lazyterm" },
      },
    },
  },
}
