-- lua/lib/mode_color.lua
-- Drives the mode block's color (StslMode bg, StslModeSep fg, StslCapL fg)
-- and animates transitions between modes with a 6-frame RGB interpolation.
-- Colors are derived from the current colorscheme's standard HL groups —
-- works with any theme.

local M = {}

local function get(name, attr, fallback)
  local h = vim.api.nvim_get_hl(0, { name = name, link = false })
  local v = h[attr]
  if type(v) == "number" then return string.format("#%06x", v) end
  return fallback
end

local P = {}  -- derived palette cache

local function derive()
  P.bg_end  = get("Normal", "bg", "#000000")
  P.bg_mid  = get("StatusLine", "bg", nil) or get("CursorLine", "bg", P.bg_end)
  P.red     = get("DiagnosticError", "fg", nil) or get("Error", "fg", "#ff5555")
  P.green   = get("String",   "fg", "#50fa7b")
  P.teal    = get("Type",     "fg", nil) or get("Special", "fg", "#8be9fd")
  P.peach   = get("Number",   "fg", nil) or get("Constant", "fg", "#ffb86c")
  P.mauve   = get("Keyword",  "fg", nil) or get("Statement", "fg", "#ff79c6")
  P.blue    = get("Function", "fg", "#8be9fd")
end

local function COLORS()
  derive()
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
  local c = COLORS()
  return c[mode] or c.n
end

function M.current() return current_hex or P.red or "#ff5555" end

local function hex_to_rgb(hex)
  return tonumber(hex:sub(2, 3), 16), tonumber(hex:sub(4, 5), 16), tonumber(hex:sub(6, 7), 16)
end

local function rgb_to_hex(r, g, b)
  return string.format("#%02x%02x%02x", math.floor(r + 0.5), math.floor(g + 0.5), math.floor(b + 0.5))
end

function M._interpolate(from, to, t)
  local fr, fg, fb = hex_to_rgb(from)
  local tr, tg, tb = hex_to_rgb(to)
  return rgb_to_hex(fr + (tr - fr) * t, fg + (tg - fg) * t, fb + (tb - fb) * t)
end

local function set_hl(hex)
  current_hex = hex
  -- Mode block: dark fg on mode-colored bg (powerline style)
  vim.api.nvim_set_hl(0, "StslMode",    { fg = P.bg_end, bg = hex, bold = true })
  -- Slant separator from mode to mid section
  vim.api.nvim_set_hl(0, "StslModeSep", { fg = hex, bg = P.bg_mid })
  -- Left cap chevron
  vim.api.nvim_set_hl(0, "StslCapL",    { fg = hex, bg = P.bg_end })
end

function M.animate(target_mode)
  local target_hex = M.target(target_mode)
  if target_hex == current_hex then return end
  if not vim.g.status_animate then set_hl(target_hex); return end

  if timer then timer:stop(); timer:close(); timer = nil end
  local from = current_hex or target_hex
  local frame = 0
  timer = assert(vim.uv.new_timer())
  timer:start(FRAME_MS, FRAME_MS, vim.schedule_wrap(function()
    frame = frame + 1
    local t = frame / FRAMES
    if t >= 1 then
      set_hl(target_hex)
      timer:stop(); timer:close(); timer = nil
    else
      set_hl(M._interpolate(from, target_hex, t))
    end
    pcall(vim.cmd, "redrawstatus")
  end))
end

function M.setup()
  if vim.g.status_animate == nil then vim.g.status_animate = true end
  derive()
  set_hl(P.red)
  vim.api.nvim_create_autocmd("ModeChanged", {
    group = vim.api.nvim_create_augroup("Lib.mode_color", { clear = true }),
    callback = function() M.animate(vim.fn.mode()) end,
  })
  -- Re-derive on theme change
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("Lib.mode_color.theme", { clear = true }),
    callback = function() derive(); set_hl(M.target(vim.fn.mode())) end,
  })
end

return M
