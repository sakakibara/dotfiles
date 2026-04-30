-- lua/config/init.lua
local M = {}

-- Pre-noice vim.notify tee. See M.setup() header for the full story.
local _pending = {}
local SILENT_KIND = "lib_tee"

function M.setup()
  -- Diagnostic: capture every vim.notify and vim.api.nvim_echo call from
  -- the very start of setup() for 5 minutes, with source-callsite
  -- traceback. Output: /tmp/pack-flash-trace.log. Remove once the brief
  -- top-of-screen flash source is identified.
  do
    local logf = io.open("/tmp/pack-flash-trace.log", "w")
    if logf then
      local function log(line)
        logf:write(("[%s] %s\n"):format(os.date("%H:%M:%S"), line))
        logf:flush()
      end
      log("=== flash trace started (M.setup begin) ===")
      local orig_notify = vim.notify
      vim.notify = function(msg, level, opts)
        log(("notify: %s"):format(tostring(msg):sub(1, 120)))
        log("  trace: " .. (debug.traceback("", 2):gsub("\n", " | ")))
        return orig_notify(msg, level, opts)
      end
      local orig_echo = vim.api.nvim_echo
      vim.api.nvim_echo = function(chunks, history, opts)
        local text = ""
        for _, c in ipairs(chunks or {}) do text = text .. (c[1] or "") end
        log(("echo (history=%s): %s"):format(tostring(history), text:sub(1, 120)))
        log("  trace: " .. (debug.traceback("", 2):gsub("\n", " | ")))
        return orig_echo(chunks, history, opts)
      end
      vim.defer_fn(function()
        vim.notify = orig_notify
        vim.api.nvim_echo = orig_echo
        log("=== flash trace ended ===")
        logf:close()
      end, 300000)
    end
  end

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
    -- Wrapper A no longer echoes. Echoing populates :messages but also
    -- briefly renders to the cmdline / hit-enter-prompts on volume,
    -- which interferes with the cold-install splash and produces the
    -- "message flashes at top of screen then toast appears" artifact.
    -- Just queue for diagnostic recovery; downstream wrappers (noice,
    -- snacks) will handle visible rendering once they attach.
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
        if ok_mgr then
          local orig_add = Manager.add
          Manager.add = function(m)
            if m.event == "msg_show" and m.kind == SILENT_KIND then
              return
            end
            return orig_add(m)
          end
        end
        -- Wrapper B used to echo to :messages via nvim_echo with a
        -- SILENT_KIND filter so noice would dedup. But the nvim_echo
        -- still produced a brief cmdline flash before noice's filter
        -- could suppress the rendering — visible as "message at top of
        -- screen" right before the toast. snacks/noice already record
        -- their own histories, so we drop the echo entirely. The
        -- queued _pending list is dropped too: replaying through noice
        -- would render every queued install-time notification as a
        -- toast after the splash closes (an explicit non-goal).
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
