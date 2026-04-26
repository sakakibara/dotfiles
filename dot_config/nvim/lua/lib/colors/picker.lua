-- lua/lib/colors/picker.lua
-- Floating-window color picker. State machine: closed → compact → expanded.
-- Window creation and rendering arrive in Task 4; this scaffold only
-- exposes the state lifecycle so harmony/recents/etc. can be wired up
-- without depending on UI.
local C = require("lib.colors.color")
local M = {}

local function default_color()
  return C.from_hex("#ffffff")
end

-- opts:
--   initial = Color (defaults to white)
--   anchor  = { buf, lnum, col_s, col_e } — write-back range (commit in Task 8)
function M.open(opts)
  opts = opts or {}
  local color = opts.initial or default_color()
  local state = {
    color   = color,
    mode    = "compact",   -- closed | compact | expanded
    space   = "rgb",       -- rgb | hsl | oklch
    slider  = 1,           -- 1..3 within the active space
    anchor  = opts.anchor,
    buf     = nil,         -- floating buffer (Task 4)
    win     = nil,         -- floating window (Task 4)
  }
  return state
end

function M.close(state)
  if state.win and vim.api.nvim_win_is_valid(state.win) then
    vim.api.nvim_win_close(state.win, true)
  end
  if state.buf and vim.api.nvim_buf_is_valid(state.buf) then
    vim.api.nvim_buf_delete(state.buf, { force = true })
  end
  state.mode = "closed"
end

function M.toggle_expand(state)
  if state.mode == "compact" then
    state.mode = "expanded"
  elseif state.mode == "expanded" then
    state.mode = "compact"
  end
  -- Re-rendering hooks in Task 4+.
end

return M
