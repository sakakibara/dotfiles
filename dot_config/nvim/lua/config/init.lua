-- lua/config/init.lua
local M = {}

function M.setup()
  -- Stage 1: early, synchronous
  require("lib").init()
  require("core.profile").start()

  require("config.options")

  -- Autocmds load unconditionally so our SwapExists handler is in place
  -- before any buffer (argument file, :e, session restore) is ever read.
  -- The old has_file gate left a race window where a fast :e could beat
  -- the VeryLazy load.
  require("config.autocmds")

  -- Stage 2: plugin boot. Eager plugins load by priority (catppuccin is
  -- priority=1000, so its config runs and sets the colorscheme before
  -- anything else). Doing this BEFORE UI chrome setup means apply_hl()
  -- resolves against the final palette on first call — no frame-one
  -- flicker when the themed highlights re-apply.
  require("core.pack").setup({ specs = require("config.plugins") })

  -- UI chrome. Runs synchronously after plugins so the first paint
  -- (after VimEnter) already has correct winbar / statusline / tabline
  -- / statuscolumn rendered with the colorscheme's colors.
  Lib.statusline.setup()
  Lib.winbar.setup()
  Lib.statuscolumn.setup()
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
