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
