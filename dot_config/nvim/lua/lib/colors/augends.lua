-- lua/lib/colors/augends.lua
-- dial.nvim augends for OKLCH channel adjustment of color literals under
-- the cursor. <C-a>/<C-x> bump the channel, the literal is rewritten in
-- its original surface format (hex stays hex, rgb stays rgb, etc.).
--
-- Usage in dial config:
--
--   local A = require("lib.colors.augends")
--   require("dial.config").augends:register_group({
--     default = {
--       A.lightness({ step = 0.05 }),  -- L: 0..1
--       A.chroma   ({ step = 0.02 }),  -- C: 0..~0.4
--       A.hue      ({ step = 5    }),  -- h: 0..360 degrees
--       -- ... other augends ...
--     },
--   })
--
-- The augend interface (find/add) follows dial.nvim's own contract; see
-- https://github.com/monaqa/dial.nvim/blob/master/doc/dial.txt for the
-- shape.

local C      = require("lib.colors.color")
local F      = require("lib.colors.format")
local parse  = require("lib.colors.parse")
local M      = {}

-- Find the color literal that contains the cursor. dial uses 1-indexed
-- byte columns where `from <= cursor <= to`. parse.parse uses 0-indexed
-- byte offsets.
local function find_color(line, cursor)
  local col0 = (cursor or 1) - 1
  local hit  = parse.parse(line, col0)
  if not hit then return nil end
  -- Some literals (e.g. CSS named colors with no spelled fmt) might lack
  -- a usable source — only operate on those format.format can serialize.
  local fmt = (hit.color.source and hit.color.source.fmt) or "hex"
  if not F.is_format(fmt) then return nil end
  return {
    from = hit.range.col_s + 1,
    to   = hit.range.col_e,
    text = line:sub(hit.range.col_s + 1, hit.range.col_e),
    hit  = hit,
  }
end

-- Apply `delta` to one channel of an OKLCH-derived color. `which` selects
-- the channel: "L" (lightness, 0..1), "C" (chroma, 0..~0.4 typical),
-- or "h" (hue, 0..360 wraps).
local function bump_channel(color, which, delta)
  local L, Cval, h = C.to_oklch(color)
  if which == "L" then
    L = math.max(0, math.min(1, L + delta))
  elseif which == "C" then
    Cval = math.max(0, Cval + delta)
  else  -- "h"
    h = (h + delta) % 360
    if h < 0 then h = h + 360 end
  end
  local out = C.from_oklch(L, Cval, h, color.a)
  -- Preserve the source's surface syntax for round-trip stability.
  out.source = color.source
  return out
end

-- Build an augend that bumps `channel` ("L"/"C"/"h") by `step` per
-- <C-a>/<C-x>. Multipliers (e.g. 5<C-a>) are passed in via `addend`.
local function make_augend(channel, step)
  return {
    find = function(line, cursor) return find_color(line, cursor) end,
    add = function(text, addend, cursor)
      -- dial passes the text we returned from `find`. Re-parse to get
      -- the color object back (parse needs the line + an offset; we
      -- treat the literal as a one-line input with cursor at 0).
      local hit = parse.parse(text, 0)
      if not hit then return { text = text, cursor = cursor } end
      local bumped = bump_channel(hit.color, channel, addend * step)
      local fmt    = (hit.color.source and hit.color.source.fmt) or "hex"
      local new    = F.format(bumped, fmt, hit.color.source)
      -- Place cursor at the end of the new text (dial convention for
      -- non-cyclic augends).
      return { text = new, cursor = #new }
    end,
  }
end

function M.lightness(opts) return make_augend("L", (opts or {}).step or 0.05) end
function M.chroma   (opts) return make_augend("C", (opts or {}).step or 0.02) end
function M.hue      (opts) return make_augend("h", (opts or {}).step or 5)    end

return M
