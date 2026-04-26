-- lua/lib/colors/picker.lua
-- Floating-window color picker. State machine: closed → compact → expanded.
local C = require("lib.colors.color")
local H = require("lib.colors.harmony")
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
  -- Block characters (U+2588) fill the cell entirely with FG, so we must set
  -- fg = the color (not bg). bg is invisible behind a full-block glyph.
  vim.api.nvim_set_hl(0, "LibColorsPickerSwatch_" .. short, { fg = hex })
  return short
end

local function default_color()
  return C.from_hex("#ffffff")
end

local function slider_bar(value, vmax, width)
  width = width or 20
  local pos = math.floor((value / vmax) * (width - 1) + 0.5)
  pos = math.max(0, math.min(width - 1, pos))
  local out = {}
  for i = 0, width - 1 do
    out[#out + 1] = (i == pos) and "●" or "━"
  end
  return table.concat(out)
end

local function space_label(state)
  if state.space == "rgb"   then return "[rgb] hsl oklch" end
  if state.space == "hsl"   then return "rgb [hsl] oklch" end
  return "rgb hsl [oklch]"
end

-- Render the compact view as a list of lines. Live swatch coloring lands
-- via an extmark applied on top of line index 1.
local function compact_lines(state)
  local hex  = C.to_hex({ r = state.color.r, g = state.color.g, b = state.color.b, a = 1 })
  local mark = function(idx) return state.slider == idx and "▸" or " " end

  local lines = {
    "  " .. hex,                                       -- 0: header
    "  " .. string.rep("█", 38),                       -- 1: swatch (38 blocks → symmetric 2-cell margin in 42-col window)
    "",                                                -- 2: blank
  }

  if state.space == "rgb" then
    local r = math.floor(state.color.r * 255 + 0.5)
    local g = math.floor(state.color.g * 255 + 0.5)
    local b = math.floor(state.color.b * 255 + 0.5)
    table.insert(lines, string.format("  %s R %s %3d", mark(1), slider_bar(r, 255), r))
    table.insert(lines, string.format("  %s G %s %3d", mark(2), slider_bar(g, 255), g))
    table.insert(lines, string.format("  %s B %s %3d", mark(3), slider_bar(b, 255), b))
  elseif state.space == "hsl" then
    local h, s, l = C.to_hsl(state.color)
    local hi, si, li = math.floor(h), math.floor(s * 100), math.floor(l * 100)
    table.insert(lines, string.format("  %s H %s %3d", mark(1), slider_bar(hi, 360), hi))
    table.insert(lines, string.format("  %s S %s %3d", mark(2), slider_bar(si, 100), si))
    table.insert(lines, string.format("  %s L %s %3d", mark(3), slider_bar(li, 100), li))
  else
    local L, Cval, h = C.to_oklch(state.color)
    table.insert(lines, string.format("  %s L %s %.2f", mark(1), slider_bar(L * 100, 100), L))
    table.insert(lines, string.format("  %s C %s %.2f", mark(2), slider_bar(Cval * 250, 100), Cval))
    table.insert(lines, string.format("  %s H %s %3d",  mark(3), slider_bar(h, 360),         math.floor(h)))
  end

  table.insert(lines, "")                              -- blank
  -- Footer with key hints. Keys are highlighted via extmark in render() so
  -- they stand out from their description words.
  table.insert(lines, "  " .. space_label(state) .. "   Tab cycle")
  table.insert(lines, "  y yank   ⏎ commit   ⎋ cancel   q close")
  return lines
end

local function expanded_lines(state)
  local hex = C.to_hex({ r = state.color.r, g = state.color.g, b = state.color.b, a = 1 })
  local h, sat, l = C.to_hsl(state.color)
  local L, Cval, hh = C.to_oklch(state.color)

  local lines = {
    "  " .. hex,
    string.rep("█", 50),
    "",
    string.format("  hex     %s", hex),
    string.format("  rgb     rgb(%d %d %d)",
      math.floor(state.color.r * 255 + 0.5),
      math.floor(state.color.g * 255 + 0.5),
      math.floor(state.color.b * 255 + 0.5)),
    string.format("  hsl     hsl(%d %d%% %d%%)", math.floor(h), math.floor(sat * 100), math.floor(l * 100)),
    string.format("  oklch   oklch(%.3f %.3f %.1f)", L, Cval, hh),
    "",
    "  ── Suggest ───────────────────────────────",
  }

  local tw_name = H.nearest_tailwind(state.color)
  local nm_name = H.nearest_named(state.color)
  table.insert(lines, "  Tailwind  " .. (tw_name or "—"))
  table.insert(lines, "  Named     " .. (nm_name or "—"))

  if #M._recents > 0 then
    local r = {}
    for i = 1, math.min(5, #M._recents) do r[#r + 1] = M._recents[i] end
    table.insert(lines, "  Recents   " .. table.concat(r, "  "))
  end

  local comp = H.complementary(state.color)
  local tri_a, tri_b = H.triad(state.color)
  local function hexof(c) return C.to_hex({ r = c.r, g = c.g, b = c.b, a = 1 }) end
  table.insert(lines, "  Comp      " .. hexof(comp))
  table.insert(lines, "  Triad     " .. hexof(tri_a) .. "  " .. hexof(tri_b))
  table.insert(lines, "")
  table.insert(lines, "  <C-e> compact  <CR> commit  <Esc> cancel")
  return lines
end

local function render(state)
  if not state.buf or not vim.api.nvim_buf_is_valid(state.buf) then return end
  local lines = state.mode == "compact" and compact_lines(state) or expanded_lines(state)
  vim.bo[state.buf].modifiable = true
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, lines)
  vim.api.nvim_buf_clear_namespace(state.buf, M.ns, 0, -1)
  local short = ensure_swatch_hl(state.color)
  vim.api.nvim_buf_set_extmark(state.buf, M.ns, 1, 0, {
    end_row  = 1,
    end_col  = #lines[2],
    hl_group = "LibColorsPickerSwatch_" .. short,
  })
  -- Active slider row — bold highlight so the active component stands out.
  if state.mode == "compact" then
    local active_row = 3 + (state.slider - 1)  -- 0-indexed buffer row
    vim.api.nvim_set_hl(0, "LibColorsPickerActive", { bold = true })
    vim.api.nvim_buf_set_extmark(state.buf, M.ns, active_row, 0, {
      end_row  = active_row,
      end_col  = #lines[active_row + 1],  -- lua 1-indexed
      hl_group = "LibColorsPickerActive",
    })
    -- Key hint footer: highlight only the key TOKENS (Tab, y, ⏎, ⎋, q) so they
    -- stand out from their action words. Footer is the last 2 lines.
    vim.api.nvim_set_hl(0, "LibColorsPickerKey", { fg = "#f5c2e7", bold = true })
    local function hl_key(row_idx, key)
      local line = lines[row_idx + 1]  -- lua 1-indexed
      if not line then return end
      local s, e = line:find(key, 1, true)
      if s then
        vim.api.nvim_buf_set_extmark(state.buf, M.ns, row_idx, s - 1, {
          end_row = row_idx, end_col = e,
          hl_group = "LibColorsPickerKey",
        })
      end
    end
    local last = #lines - 1   -- 0-indexed of last line ("y yank ⏎ commit ⎋ cancel q close")
    local prev = last - 1     -- 0-indexed of penultimate line ("[xxx] hsl oklch  Tab cycle")
    hl_key(prev, "Tab")
    hl_key(last, "y")
    hl_key(last, "⏎")
    hl_key(last, "⎋")
    -- "q close" — match the standalone "q" with surrounding spaces to avoid
    -- matching "q" inside "cancel" or other words.
    do
      local line = lines[last + 1] or ""
      local s, e = line:find(" q ", 1, true)
      if s then
        vim.api.nvim_buf_set_extmark(state.buf, M.ns, last, s, {
          end_row = last, end_col = s + 1,
          hl_group = "LibColorsPickerKey",
        })
      end
    end
    -- Color channel labels in their respective hues
    local label_palette = {
      rgb   = { ["R"] = "#f38ba8", ["G"] = "#a6e3a1", ["B"] = "#89b4fa" },
      hsl   = { ["H"] = "#cba6f7", ["S"] = "#f9e2af", ["L"] = "#cdd6f4" },
      oklch = { ["L"] = "#cdd6f4", ["C"] = "#f9e2af", ["H"] = "#cba6f7" },
    }
    local space_labels = state.space == "rgb" and {"R","G","B"}
                      or state.space == "hsl" and {"H","S","L"}
                      or {"L","C","H"}
    for i, label in ipairs(space_labels) do
      local fg = label_palette[state.space][label]
      vim.api.nvim_set_hl(0, "LibColorsPickerLabel_" .. label, { fg = fg, bold = true })
      local row = 3 + (i - 1)  -- slider rows start at buffer row 3
      -- Label char is at byte 4 in the line: "  " (2) + mark (1) + " " (1)
      vim.api.nvim_buf_set_extmark(state.buf, M.ns, row, 4, {
        end_row  = row,
        end_col  = 5,
        hl_group = "LibColorsPickerLabel_" .. label,
      })
    end
  end
  vim.bo[state.buf].modifiable = false

  if state.win and vim.api.nvim_win_is_valid(state.win) then
    -- Resize only — DO NOT re-pass `relative`/`row`/`col`. The picker IS the
    -- current window, so `relative = "cursor"` would re-anchor to the
    -- picker's own cursor and drift on every keypress.
    local cur = vim.api.nvim_win_get_config(state.win)
    local want_w, want_h = (state.mode == "expanded") and 52 or 42, (state.mode == "expanded") and 18 or 10
    if cur.width ~= want_w or cur.height ~= want_h then
      vim.api.nvim_win_set_config(state.win, { width = want_w, height = want_h })
    end
    -- Move cursor to the active slider line so the user has a clear positional
    -- indicator beyond the ▸ marker. Slider lines start at row 3 (0-indexed)
    -- in compact mode (header, swatch, blank, then slider1/2/3). Expanded
    -- mode is read-only, so park cursor at row 0 there.
    local row = state.mode == "compact" and (3 + state.slider - 1) or 0
    pcall(vim.api.nvim_win_set_cursor, state.win, { row + 1, 0 })
  end
end

function M.open(opts)
  if #M._recents == 0 then M._recents_load() end
  opts = opts or {}
  local color = opts.initial or default_color()
  local state = {
    color      = color,
    source_fmt = color.source and color.source.fmt or "hex",
    source     = vim.deepcopy(color.source or { fmt = "hex" }),
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
    width     = 42,
    height    = 10,
    style     = "minimal",
    border    = "rounded",
    title     = " color ",
    title_pos = "left",
  })
  render(state)
  local map = function(lhs, fn)
    vim.keymap.set("n", lhs, fn, { buffer = state.buf, silent = true, nowait = true })
  end
  -- ±5 per press: visible jump in any color space (~2% RGB byte, 5% HSL, 0.05 oklch).
  -- Hold shift for ±20.
  map("h",     function() M.adjust(state, -5) end)
  map("l",     function() M.adjust(state,  5) end)
  map("H",     function() M.adjust(state, -20) end)
  map("L",     function() M.adjust(state,  20) end)
  map("j",     function() M.cycle_slider(state) end)
  map("k",     function() state.slider = ((state.slider - 2) % 3) + 1; render(state) end)
  map("y",     function()
    local hex = C.to_hex({ r = state.color.r, g = state.color.g, b = state.color.b, a = 1 })
    vim.fn.setreg("+", hex)
    vim.fn.setreg("\"", hex)
    vim.notify("Yanked " .. hex, vim.log.levels.INFO)
  end)
  map("<Tab>", function() M.cycle_space(state) end)
  map("<C-e>", function() M.toggle_expand(state) end)
  map("<CR>",  function() M.commit(state) end)
  map("<Esc>", function() M.close(state) end)
  map("q",     function() M.close(state) end)
  return state
end

function M.commit(state)
  if state.anchor and vim.api.nvim_buf_is_valid(state.anchor.buf) then
    local a = state.anchor
    local fmt = state.source_fmt or "hex"
    local text
    if fmt == "rgb" then
      local src       = state.source or {}
      local fn        = src.fn_name or (src.with_alpha and "rgba" or "rgb")
      local sep       = src.commas and ", " or " "
      local r = math.floor(state.color.r * 255 + 0.5)
      local g = math.floor(state.color.g * 255 + 0.5)
      local b = math.floor(state.color.b * 255 + 0.5)
      local a = state.color.a or 1
      if src.with_alpha then
        if src.commas then
          text = string.format("%s(%d, %d, %d, %s)", fn, r, g, b,
            a == 1 and "1" or string.format("%.2f", a))
        else
          text = string.format("%s(%d %d %d / %s)", fn, r, g, b,
            a == 1 and "1" or string.format("%.2f", a))
        end
      else
        text = string.format("%s(%d%s%d%s%d)", fn, r, sep, g, sep, b)
      end
    elseif fmt == "hsl" then
      local h, s, l = C.to_hsl(state.color)
      local src = state.source or {}
      local fn = src.fn_name or (src.with_alpha and "hsla" or "hsl")
      local sep = src.commas and ", " or " "
      local hi, si, li = math.floor(h), math.floor(s * 100), math.floor(l * 100)
      if src.with_alpha then
        local a = state.color.a or 1
        local a_str = a == 1 and "1" or string.format("%.2f", a)
        if src.commas then
          text = string.format("%s(%d, %d%%, %d%%, %s)", fn, hi, si, li, a_str)
        else
          text = string.format("%s(%d %d%% %d%% / %s)", fn, hi, si, li, a_str)
        end
      else
        text = string.format("%s(%d%s%d%%%s%d%%)", fn, hi, sep, si, sep, li)
      end
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
