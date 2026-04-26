-- lua/lib/colors/picker.lua
-- Floating-window color picker. State machine: closed → compact → expanded.
local C = require("lib.colors.color")
local M = {}

M.ns = vim.api.nvim_create_namespace("lib.colors.picker")

local function ensure_swatch_hl(color)
  local hex = C.to_hex({ r = color.r, g = color.g, b = color.b, a = 1 })
  local short = hex:sub(2)  -- strip "#"
  vim.api.nvim_set_hl(0, "LibColorsPickerSwatch_" .. short, {
    bg = hex,
    fg = C.contrast_text(color),
  })
  return short
end

local function default_color()
  return C.from_hex("#ffffff")
end

-- Render the compact view as a list of lines. Live swatch coloring lands
-- in Task 5 via an extmark applied on top of line index 1.
local function compact_lines(state)
  local hex = C.to_hex({ r = state.color.r, g = state.color.g, b = state.color.b, a = 1 })
  local lines = {
    " " .. hex,
    string.rep("█", 28),
    "",
  }
  if state.space == "rgb" then
    table.insert(lines, string.format("R %3d", math.floor(state.color.r * 255 + 0.5)))
    table.insert(lines, string.format("G %3d", math.floor(state.color.g * 255 + 0.5)))
    table.insert(lines, string.format("B %3d", math.floor(state.color.b * 255 + 0.5)))
  elseif state.space == "hsl" then
    local h, s, l = C.to_hsl(state.color)
    table.insert(lines, string.format("H %3d", math.floor(h)))
    table.insert(lines, string.format("S %3d", math.floor(s * 100)))
    table.insert(lines, string.format("L %3d", math.floor(l * 100)))
  else
    local L, Cval, h = C.to_oklch(state.color)
    table.insert(lines, string.format("L %.2f", L))
    table.insert(lines, string.format("C %.2f", Cval))
    table.insert(lines, string.format("H %3d", math.floor(h)))
  end
  table.insert(lines, "")
  table.insert(lines, "[" .. state.space .. "] <Tab> cycle  <CR> commit  <Esc> cancel  <C-e> expand")
  return lines
end

local function render(state)
  if not state.buf or not vim.api.nvim_buf_is_valid(state.buf) then return end
  local lines = state.mode == "compact" and compact_lines(state) or compact_lines(state)
  vim.bo[state.buf].modifiable = true
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, lines)
  vim.api.nvim_buf_clear_namespace(state.buf, M.ns, 0, -1)
  local short = ensure_swatch_hl(state.color)
  vim.api.nvim_buf_set_extmark(state.buf, M.ns, 1, 0, {
    end_row  = 1,
    end_col  = #lines[2],
    hl_group = "LibColorsPickerSwatch_" .. short,
  })
  vim.bo[state.buf].modifiable = false
end

function M.open(opts)
  opts = opts or {}
  local color = opts.initial or default_color()
  local state = {
    color  = color,
    mode   = "compact",
    space  = "rgb",
    slider = 1,
    anchor = opts.anchor,
    buf    = nil,
    win    = nil,
  }
  state.buf = vim.api.nvim_create_buf(false, true)
  vim.bo[state.buf].buftype   = "nofile"
  vim.bo[state.buf].bufhidden = "wipe"
  state.win = vim.api.nvim_open_win(state.buf, true, {
    relative  = "cursor",
    row       = 1,
    col       = 0,
    width     = 32,
    height    = 9,
    style     = "minimal",
    border    = "rounded",
    title     = " color ",
    title_pos = "left",
  })
  render(state)
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
  render(state)
end

-- Adjust the active component by `delta`. Units are RGB-byte for rgb,
-- degrees for HSL hue / oklch hue, percent-points for HSL S/L, and
-- 0.01-units for oklch L/C.
function M.adjust(state, delta)
  local space = state.space
  local s = state.slider
  if space == "rgb" then
    local v = { state.color.r, state.color.g, state.color.b }
    local raw = math.floor(v[s] * 255 + 0.5) + delta
    v[s] = math.max(0, math.min(255, raw)) / 255
    state.color = {
      r = v[1], g = v[2], b = v[3], a = state.color.a or 1,
      space = "srgb", source = state.color.source,
    }
  elseif space == "hsl" then
    local h, sat, l = C.to_hsl(state.color)
    if s == 1 then
      h = (h + delta) % 360
    elseif s == 2 then
      sat = math.max(0, math.min(1, sat + delta / 100))
    else
      l = math.max(0, math.min(1, l + delta / 100))
    end
    state.color = C.from_hsl(h, sat, l, state.color.a)
  else
    local L, Cval, h = C.to_oklch(state.color)
    if s == 1 then
      L = math.max(0, math.min(1, L + delta / 100))
    elseif s == 2 then
      Cval = math.max(0, math.min(0.4, Cval + delta / 100))
    else
      h = (h + delta) % 360
    end
    state.color = C.from_oklch(L, Cval, h, state.color.a)
  end
  render(state)
end

function M.cycle_slider(state)
  state.slider = (state.slider % 3) + 1
  render(state)
end

function M.cycle_space(state)
  local order = { rgb = "hsl", hsl = "oklch", oklch = "rgb" }
  state.space = order[state.space]
  state.slider = 1
  render(state)
end

return M
