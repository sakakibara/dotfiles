local M = {}

local common = require("core.pack.ui._common")

M.keymaps         = common.keymaps
M.pad_glyph       = common.pad_glyph
M.lock_pack_window = common.lock_pack_window
M.plan_columns    = common.plan_columns

-- Currently-open cold-install splash, exposed so external callers
-- (nvim-treesitter logger override, blink download notifier) can pipe
-- progress text into the splash status line via :set_status_text.
-- Set/cleared by the splash module.
M._active_splash = nil

M.update_review     = require("core.pack.ui.update_review").update_review
M.clean_review      = require("core.pack.ui.clean_review").clean_review
M.rollback_review   = require("core.pack.ui.rollback_review").rollback_review
M.status            = require("core.pack.ui.status").status
M.fidget            = require("core.pack.ui.fidget").fidget
M.cold_install_splash = require("core.pack.ui.splash").cold_install_splash

return M
