local M = {}

-- Pre-noice vim.notify tee. See M.setup() header for the full story.
local _pending = {}
local SILENT_KIND = "lib_tee"

function M.setup()
  -- Wrapper A: install a queueing vim.notify synchronously, BEFORE
  -- require("lib").init() and core.pack.setup — before any plugin, autocmd,
  -- or buffer load can fire vim.notify.
  --
  -- The window this closes: noice's `ext_messages` attach is scheduled from
  -- VeryLazy. Anything calling vim.notify before noice's attach completes
  -- — FileType autocmds firing during cold-boot argv reads, core.pack
  -- internals reporting keymap conflicts, etc. — would otherwise hit
  -- nvim's cmdline backend with cmdheight=0 and trigger press-ENTER
  -- prompts that block the main thread.
  --
  -- We just queue and rely on wrapper B (in the VeryLazy callback below)
  -- to drain the queue once noice's ext_messages backend is live —
  -- replaying via nvim_echo with kind=SILENT_KIND so :messages records
  -- every pre-noice message and the SILENT_KIND filter on Manager.add
  -- prevents noice from rendering them as duplicate toasts.
  --
  -- vim.notify_once late-binds via `vim.notify` field lookup, so wrapper A
  -- catches it too; its internal `notified[msg]` cache is set on first call,
  -- which means the replay doesn't re-fire vim.notify_once — wrapper B
  -- replays through `prev` directly, bypassing the cache entirely.
  vim.notify = function(msg, level, opts)
    table.insert(_pending, { msg = msg, level = level, opts = opts })
  end

  -- Stage 1: early, synchronous
  require("lib").init()
  require("core.profile").start()

  require("config.options")

  -- Keymaps load BEFORE pack.setup so that mapleader/maplocalleader (set at
  -- the top of keymaps.lua) are in place before any eager plugin's config()
  -- registers `<leader>x` keymaps, AND so keymaps are bound from t=0 instead
  -- of after the UIEnter→VeryLazy window (~30–100 ms where rapid leader
  -- presses would otherwise sit in timeoutlen or hit a `\` literal).
  require("config.keymaps")

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
      --      with the silent-kind tee, drains _pending into :messages.
      --
      -- The Manager.add wrapper filters msg_show events tagged SILENT_KIND
      -- so the silent-kind echo (which only exists to populate :messages
      -- history) doesn't produce a duplicate :NoiceAll entry or toast.
      --
      -- Splash protection: while the cold-install splash is up, the post-
      -- noice tee skips the prev() forward — :messages still records via
      -- the silent-kind echo, but no top-right toast is rendered against
      -- the splash overlay. This preserves the original splash contract
      -- (no chrome competing with the centered box).
      --
      -- Queue drain (pre-noice messages):
      --   * INFO and below stay on the SILENT_KIND-only path — replaying
      --     every queued install-time notification as a toast burst after
      --     splash closes is an explicit non-goal. Stock noice doesn't
      --     backfill :NoiceAll with pre-attach history either.
      --   * WARN/ERROR forward through `prev` (noice). The SILENT_KIND
      --     echo path is invisible to both histories on builds where
      --     ext_messages bypasses nvim's internal :messages — and errors
      --     are rare and signal real problems, so we accept the toast/
      --     popup that comes with replay rather than risk losing them.
      local function level_to_hl(level)
        level = level or vim.log.levels.INFO
        if level >= vim.log.levels.ERROR then return "ErrorMsg" end
        if level >= vim.log.levels.WARN  then return "WarningMsg" end
        return "Normal"
      end
      vim.schedule(function()
        local ok_mgr, Manager = pcall(require, "noice.message.manager")
        if ok_mgr then
          local orig_add = Manager.add
          Manager.add = function(m)
            if m.event == "msg_show" and m.kind == SILENT_KIND then
              return
            end
            return orig_add(m)
          end
        end

        local prev = vim.notify
        vim.notify = function(msg, level, opts)
          pcall(vim.api.nvim_echo,
            { { tostring(msg), level_to_hl(level) } },
            true,
            { kind = SILENT_KIND })
          local ok_ui, UI = pcall(require, "core.pack.ui")
          if ok_ui and UI._active_splash then
            return  -- splash up: :messages-only, no toast competing with overlay
          end
          return prev(msg, level, opts)
        end

        for _, m in ipairs(_pending) do
          local lvl = m.level or vim.log.levels.INFO
          if lvl >= vim.log.levels.WARN then
            vim.schedule(function() pcall(prev, m.msg, m.level, m.opts) end)
          else
            pcall(vim.api.nvim_echo,
              { { tostring(m.msg), level_to_hl(m.level) } },
              true,
              { kind = SILENT_KIND })
          end
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
