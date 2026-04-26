-- lua/lib/colors/picker.lua
-- Floating-window color picker. State machine: closed → compact → expanded.
local C = require("lib.colors.color")
local M = {}

M.ns = vim.api.nvim_create_namespace("lib.colors.picker")

M._recents      = {}
M._recents_path = vim.fn.stdpath("state") .. "/lib-colors-recents.json"
M.RECENTS_MAX   = 32

function M._recents_load()
  local fd = io.open(M._recents_path, "r")
  if not fd then return end
  local content = fd:read("*a")
  fd:close()
  local ok, data = pcall(vim.json.decode, content)
  if ok and type(data) == "table" then M._recents = data end
end

function M._recents_save()
  vim.fn.mkdir(vim.fn.fnamemodify(M._recents_path, ":h"), "p")
  local fd = io.open(M._recents_path, "w")
  if not fd then return end
  fd:write(vim.json.encode(M._recents))
  fd:close()
end

function M._recents_push(hex)
  for i = #M._recents, 1, -1 do
    if M._recents[i] == hex then table.remove(M._recents, i) end
  end
  table.insert(M._recents, 1, hex)
  while #M._recents > M.RECENTS_MAX do
    table.remove(M._recents)
  end
end

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
  if #M._recents == 0 then M._recents_load() end
  opts = opts or {}
  local color = opts.initial or default_color()
  local state = {
    color      = color,
    source_fmt = color.source and color.source.fmt or "hex",
    mode       = "compact",
    space      = "rgb",
    slider     = 1,
    anchor     = opts.anchor,
    buf        = nil,
    win        = nil,
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
  local map = function(lhs, fn)
    vim.keymap.set("n", lhs, fn, { buffer = state.buf, silent = true, nowait = true })
  end
  map("h",     function() M.adjust(state, -1) end)
  map("l",     function() M.adjust(state,  1) end)
  map("H",     function() M.adjust(state, -10) end)
  map("L",     function() M.adjust(state,  10) end)
  map("j",     function() M.cycle_slider(state) end)
  map("k",     function() state.slider = ((state.slider - 2) % 3) + 1; render(state) end)
  map("<Tab>", function() M.cycle_space(state) end)
  map("<C-e>", function() M.toggle_expand(state) end)
  map("<CR>",  function() M.commit(state) end)
  map("<Esc>", function() M.close(state) end)
  return state
end

function M.commit(state)
  if state.anchor and vim.api.nvim_buf_is_valid(state.anchor.buf) then
    local a = state.anchor
    local fmt = state.source_fmt or "hex"
    local text
    if fmt == "rgb" then
      text = string.format("rgb(%d %d %d)",
        math.floor(state.color.r * 255 + 0.5),
        math.floor(state.color.g * 255 + 0.5),
        math.floor(state.color.b * 255 + 0.5))
    elseif fmt == "hsl" then
      local h, s, l = C.to_hsl(state.color)
      text = string.format("hsl(%d %d%% %d%%)",
        math.floor(h), math.floor(s * 100), math.floor(l * 100))
    elseif fmt == "oklch" then
      local L, Cval, h = C.to_oklch(state.color)
      text = string.format("oklch(%.3f %.3f %.1f)", L, Cval, h)
    elseif fmt == "oklab" then
      local L, Cval, h = C.to_oklch(state.color)
      local av = Cval * math.cos(math.rad(h))
      local bv = Cval * math.sin(math.rad(h))
      text = string.format("oklab(%.3f %.3f %.3f)", L, av, bv)
    else
      text = C.to_hex(state.color)
    end
    vim.api.nvim_buf_set_text(a.buf, a.lnum, a.col_s, a.lnum, a.col_e, { text })
  end
  -- Push committed hex into recents
  local hex = C.to_hex({ r = state.color.r, g = state.color.g, b = state.color.b, a = 1 })
  M._recents_push(hex)
  M._recents_save()
  M.close(state)
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
