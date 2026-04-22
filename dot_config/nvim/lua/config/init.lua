-- lua/config/init.lua
local M = {}

function M.setup()
  -- Stage 1: early, synchronous
  require("lib").init()
  require("core.profile").start()

  require("config.options")

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

      -- Tee vim.notify into native :messages without producing the
      -- duplicate-toast / duplicate-:Noice-all-entry problem a naïve
      -- wrapper causes.
      --
      -- The problem with a naïve `nvim_echo(..., true, ..); prev(...)`
      -- wrapper: noice has ext_messages attached, so every nvim_echo
      -- fires an msg_show event that noice stores in its Manager AND
      -- routes through its default `msg_show` / `warning` / `error`
      -- routes — all of which resolve to the same "notify" view that
      -- the `event = "notify"` route (fired by the `prev(...)` call)
      -- also uses. Result: two Manager entries (visible in :Noice all)
      -- and two snacks toasts per vim.notify call.
      --
      -- The fix has two parts, both installed after noice's own
      -- VeryLazy schedule so `vim.notify` and the Manager module are the
      -- noice-owned versions:
      --   1. Tag our tee'd nvim_echo with a custom `kind` string.
      --      Nvim's history is populated from `add_msg_hist` independent
      --      of the UI layer, so :messages gets the entry regardless of
      --      whether noice stores/routes it.
      --   2. Wrap `noice.message.manager.add` to drop msg_show events
      --      with that kind. This is the ONLY place noice records a
      --      Message, so no entry reaches :Noice all and no route ever
      --      resolves to a toast view. The `prev(...)` call afterward
      --      produces the single `event = "notify"` message that hits
      --      :Noice all once and toasts once via noice's snacks backend.
      vim.schedule(function()
        local ok_mgr, Manager = pcall(require, "noice.message.manager")
        if ok_mgr then
          local SILENT_KIND = "lib_tee"
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
        end
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
