-- lua/config/init.lua
local M = {}

-- Pre-noice vim.notify tee. See M.setup() header for the full story.
local _pending = {}
local SILENT_KIND = "lib_tee"

function M.setup()
  -- Wrapper A: install vim.notify tee synchronously, BEFORE require("lib").init()
  -- and core.pack.setup — before any plugin, autocmd, or buffer load can fire
  -- vim.notify.
  --
  -- The window this closes: noice's `ext_messages` attach is scheduled from
  -- VeryLazy (noice/init.lua:~34 calls vim.schedule(load)), and wrapper B
  -- below is also scheduled from our VeryLazy+schedule. Anything calling
  -- vim.notify before wrapper B runs — FileType autocmds firing during cold-
  -- boot argv reads, `:e foo.tsx` via oil while noice is mid-attach, core.pack
  -- internals reporting keymap conflicts, etc. — bypasses noice entirely. The
  -- historical symptom: a warning flashes in the cmdline briefly (truncated
  -- by press-ENTER), never reaches :Noice all or toasts, and if emitted via
  -- vim.notify_once, never fires again the rest of the session.
  --
  -- Wrapper A echoes with history=true so :messages is populated immediately,
  -- and queues {msg, level, opts}. When noice attaches (see handler below),
  -- its `load` replaces vim.notify with noice's handler — wrapper A becomes
  -- unreachable. Wrapper B then re-installs a tee on top of noice's vim.notify
  -- and drains the queue through noice, so :Noice all and the toast backend
  -- get every pre-noice message.
  --
  -- vim.notify_once late-binds via `vim.notify` field lookup, so wrapper A
  -- catches it too; its internal `notified[msg]` cache is set on first call,
  -- which means the replay through noice doesn't re-fire vim.notify_once —
  -- we replay through `prev` directly, bypassing the cache entirely.
  vim.notify = function(msg, level, opts)
    local lvl = type(level) == "number" and level or vim.log.levels.INFO
    local hl = (lvl >= vim.log.levels.ERROR and "ErrorMsg")
      or (lvl >= vim.log.levels.WARN and "WarningMsg")
      or "Normal"
    local text = tostring(msg or "")
    if type(opts) == "table" and opts.title then
      text = ("[%s] %s"):format(opts.title, text)
    end
    -- No kind on this echo: noice isn't attached yet, so there's no Manager
    -- to dedup against and nothing to route. Just populate :messages.
    pcall(vim.api.nvim_echo, { { text, hl } }, true, {})
    table.insert(_pending, { msg = msg, level = level, opts = opts })
  end

  -- Stage 1: early, synchronous
  require("lib").init()
  require("core.profile").start()

  require("config.options")

  -- chezmoi template support: filetype detection for *.tmpl, gotmpl
  -- treesitter injection in {{...}} regions, blink.cmp source registration.
  Lib.chezmoi.setup()

  -- Autocmds load unconditionally so our SwapExists handler is in place
  -- before any buffer (argument file, :e, session restore) is ever read.
  require("config.autocmds")

  -- K must be remapped in Stage 1: if user spams K during startup before
  -- Stage 3 (VeryLazy) runs, nvim's default K → keywordprg=:Man spawns a
  -- synchronous subprocess per press and stalls the input queue. Installing
  -- globally here also suppresses nvim's LspAttach-installed buffer-local K
  -- (it only installs when maparg('K','n',...) is empty). Noice's markdown
  -- K inside hover floats is dealt with separately in plugins/ui.lua.
  vim.keymap.set("n", "K", function()
    local clients = vim.lsp.get_clients({ bufnr = 0, method = "textDocument/hover" })
    if #clients > 0 then vim.lsp.buf.hover() end
  end, { desc = "LSP hover (no-op without hover client)" })

  -- Plugins. install.colorscheme pre-applies catppuccin synchronously
  -- after install, before any eager spec's config — so chrome's apply_hl
  -- (called below) samples themed highlights the first time.
  require("core.pack").setup({
    specs   = require("config.plugins"),
    install = { colorscheme = "catppuccin" },
  })

  Lib.colors.setup({})

  -- Chrome highlights are derived (fg = Function.fg etc.) so they must
  -- be registered AFTER catppuccin applied — otherwise we'd sample
  -- default highlights and rely on ColorScheme to re-apply, which leaves
  -- a one-frame window where the bar renders with un-themed colors.
  -- Option strings (vim.o.statusline etc.) are set in options.lua so
  -- the bar layout is reserved from frame 0.
  Lib.statusline.setup()
  Lib.winbar.setup()
  Lib.tabline.setup()

  -- Stage 3: after UI ready — setups that benefit from deferring.
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    once = true,
    callback = function()
      require("config.keymaps")
      Lib.lsp.setup()
      Lib.format.setup()
      Lib.root.setup()

      -- Wrapper B: install the post-noice tee and drain wrapper A's queue.
      --
      -- Schedule ordering inside this tick, FIFO:
      --   1. noice's own `load` (scheduled from noice.setup during its
      --      VeryLazy load_spec) — attaches ext_messages and replaces
      --      vim.notify with noice's handler, discarding wrapper A.
      --   2. this callback — captures noice's vim.notify as `prev`, wraps
      --      with the silent-kind tee, drains _pending through `prev`.
      --
      -- Manager.add wrapper filters msg_show events tagged SILENT_KIND so the
      -- tee's echo doesn't produce duplicate Manager entries / duplicate
      -- toasts. Only the `prev(...)` call at the bottom reaches Manager as a
      -- normal notify event, giving exactly one :Noice all entry and one
      -- toast per vim.notify call. (See commit history on this file for the
      -- dedup derivation.)
      vim.schedule(function()
        local ok_mgr, Manager = pcall(require, "noice.message.manager")
        if not ok_mgr then return end

        local orig_add = Manager.add
        Manager.add = function(m)
          if m.event == "msg_show" and m.kind == SILENT_KIND then
            return
          end
          return orig_add(m)
        end

        local prev = vim.notify
        vim.notify = function(msg, level, opts)
          local lvl = type(level) == "number" and level or vim.log.levels.INFO
          local hl = (lvl >= vim.log.levels.ERROR and "ErrorMsg")
            or (lvl >= vim.log.levels.WARN and "WarningMsg")
            or "Normal"
          local text = tostring(msg or "")
          if type(opts) == "table" and opts.title then
            text = ("[%s] %s"):format(opts.title, text)
          end
          pcall(vim.api.nvim_echo, { { text, hl } }, true, { kind = SILENT_KIND })
          return prev(msg, level, opts)
        end

        -- Drain via `prev` directly, not wrapper B: wrapper A already echoed
        -- each entry to :messages at capture time, so routing through B would
        -- produce a duplicate echo. We only need noice to see them.
        for _, n in ipairs(_pending) do
          prev(n.msg, n.level, n.opts)
        end
        _pending = {}
      end)

      require("core.profile").dump()
    end,
  })

  -- Schedule VeryLazy dispatch on UIEnter
  require("core.event").schedule_user_events()
  -- Bridge our core.event VeryLazy into vim's User autocmd system
  require("core.event").on("VeryLazy", function()
    vim.api.nvim_exec_autocmds("User", { pattern = "VeryLazy", modeline = false })
  end)
end

return M
