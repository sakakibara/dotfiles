-- Render multi-stop swatch strips for `linear-gradient(...)`,
-- `radial-gradient(...)`, and `conic-gradient(...)` calls. Each detected
-- gradient gets a row of small `█` cells (one per stop, colored with the
-- stop's hex) appended inline after the closing paren — so the gradient
-- stays readable as text *and* you can see the actual color sequence at
-- a glance.
--
-- Triggered by the main render flow when the buffer's contrast/gradient
-- decoration is enabled (init.lua redraw); also exposed standalone via
-- `gradient.render(buf, top, bot)`.

local C     = require("lib.colors.color")
local parse = require("lib.colors.parse")
local M     = {}

M.ns       = vim.api.nvim_create_namespace("Lib.colors.gradient")
M._enabled = {}        -- per-buffer toggle: nil/true/false
M._hl_cache = {}       -- { [hex_no_hash] = true }

local PATTERNS = {
  "linear%-gradient%b()",
  "radial%-gradient%b()",
  "conic%-gradient%b()",
}

local function ensure_hl(hex_no_hash)
  if M._hl_cache[hex_no_hash] then return end
  vim.api.nvim_set_hl(0, "LibColorsGradientBg_" .. hex_no_hash, {
    bg = "#" .. hex_no_hash,
  })
  M._hl_cache[hex_no_hash] = true
end

-- Find every gradient call on the line. Returns a list of
-- `{ col_e, stops = { {color}, ... } }`.
local function find_gradients(line)
  local out = {}
  for _, pat in ipairs(PATTERNS) do
    local s, e = line:find(pat)
    while s do
      local body  = line:sub(s, e)
      local inner = body:match("%((.+)%)$") or ""
      local stops = parse.parse_all(inner)
      if #stops >= 2 then
        out[#out + 1] = { col_e = e, stops = stops }
      end
      s, e = line:find(pat, e + 1)
    end
  end
  return out
end

function M.render(buf, top, bot)
  vim.api.nvim_buf_clear_namespace(buf, M.ns, top, bot + 1)
  if not vim.api.nvim_buf_is_valid(buf) then return end

  local lines = vim.api.nvim_buf_get_lines(buf, top, bot + 1, false)
  for i, line in ipairs(lines) do
    local lnum = top + i - 1
    for _, g in ipairs(find_gradients(line)) do
      local virt = { { " ", "Normal" } }
      for _, stop in ipairs(g.stops) do
        local hex   = C.to_hex({ r = stop.color.r, g = stop.color.g, b = stop.color.b, a = 1 })
        local short = hex:sub(2)
        ensure_hl(short)
        virt[#virt + 1] = { " ", "LibColorsGradientBg_" .. short }
      end
      vim.api.nvim_buf_set_extmark(buf, M.ns, lnum, g.col_e, {
        virt_text     = virt,
        virt_text_pos = "inline",
        hl_mode       = "combine",
      })
    end
  end
end

function M.clear(buf)
  if vim.api.nvim_buf_is_valid(buf) then
    vim.api.nvim_buf_clear_namespace(buf, M.ns, 0, -1)
  end
end

function M.toggle(buf)
  if not buf or buf == 0 then buf = vim.api.nvim_get_current_buf() end
  M._enabled[buf] = not M._enabled[buf]
  if M._enabled[buf] then
    local top = 0
    local bot = math.max(0, vim.api.nvim_buf_line_count(buf) - 1)
    M.render(buf, top, bot)
  else
    M.clear(buf)
  end
  return M._enabled[buf]
end

function M.is_enabled(buf) return M._enabled[buf] == true end

return M
