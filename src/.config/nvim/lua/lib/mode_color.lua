-- Drives the mode block's color (StslMode bg, StslModeSep fg, StslCapL fg)
-- and animates transitions between modes with a 6-frame RGB interpolation.
-- Colors are derived from the current colorscheme's standard HL groups --
-- works with any theme.

local palette = require("lib.palette")

local M = {}

function M.lookup(tbl, mode)
  return tbl[mode] or tbl[mode:sub(1, 2)] or tbl[mode:sub(1, 1)]
end

local colors

local function build(P)
  return {
    n = P.red, no = P.red,
    i = P.green, ic = P.green, ix = P.green,
    v = P.teal, vs = P.teal, V = P.teal, Vs = P.teal, ["\22"] = P.teal, ["\22s"] = P.teal,
    c = P.peach, cv = P.peach,
    s = P.mauve, S = P.mauve, ["\19"] = P.mauve,
    R = P.blue, Rc = P.blue, Rx = P.blue, Rv = P.blue,
    t = P.peach,
  }
end

local FRAMES = 6
local FRAME_MS = 20

local current_hex
local timer

function M.target(mode)
  colors = colors or build(palette.get())
  return M.lookup(colors, mode) or colors.n
end

local function set_hl(hex)
  local P = palette.get()
  current_hex = hex
  -- Mode block: dark fg on mode-colored bg (powerline style)
  vim.api.nvim_set_hl(0, "StslMode",    { fg = P.bg_end, bg = hex, bold = true })
  -- Slant separator from mode to mid section
  vim.api.nvim_set_hl(0, "StslModeSep", { fg = hex, bg = P.bg_mid })
  -- Left cap chevron
  vim.api.nvim_set_hl(0, "StslCapL",    { fg = hex, bg = P.bg_end })
end

local function stop_timer()
  if timer then
    timer:stop()
    if not timer:is_closing() then timer:close() end
    timer = nil
  end
end

function M.animate(target_mode)
  local target_hex = M.target(target_mode)
  if target_hex == current_hex then return end
  if not vim.g.status_animate then set_hl(target_hex); return end

  stop_timer()
  local from = current_hex or target_hex
  local frame = 0
  -- Capture THIS animation's timer in the closure instead of referencing
  -- the module-local `timer`. Otherwise a rapidly-retriggered ModeChanged
  -- nils the module-local between the old timer's in-flight scheduled
  -- callbacks and their execution, and the callback crashes on nil.
  local my_timer = assert(vim.uv.new_timer())
  timer = my_timer
  my_timer:start(FRAME_MS, FRAME_MS, vim.schedule_wrap(function()
    if my_timer:is_closing() then return end
    frame = frame + 1
    local t = frame / FRAMES
    if t >= 1 then
      set_hl(target_hex)
      my_timer:stop(); my_timer:close()
      if timer == my_timer then timer = nil end
    else
      set_hl(palette.mix(from, target_hex, t))
    end
    pcall(vim.cmd, "redrawstatus")
  end))
end

function M.refresh()
  colors = nil
  stop_timer()
  set_hl(M.target(vim.fn.mode(1)))
end

function M.setup()
  if vim.g.status_animate == nil then vim.g.status_animate = true end
  -- Initial color = actual mode's target (not a placeholder red). Prevents
  -- a brief red flash on startup before the first ModeChanged animates.
  M.refresh()
  vim.api.nvim_create_autocmd("ModeChanged", {
    group = vim.api.nvim_create_augroup("Lib.mode_color", { clear = true }),
    callback = function() M.animate(vim.fn.mode(1)) end,
  })
end

return M
