-- lua/lib/colors/tailwind.lua
-- Tailwind v4 palette resolution: built-in defaults + project @theme overlay.
local C       = require("lib.colors.color")
local PALETTE = require("lib.colors._tailwind_palette")
local M       = {}

-- Project overlay: `--color-*` declarations harvested from project CSS files.
-- Populated by scan_file/scan_project (Task 12). Same { L, C, h } shape as
-- the built-in palette.
M._overlay = {}

-- Resolve a palette class name (e.g. "red-500") to a Color. Project overlay
-- takes precedence over the built-in palette.
function M.resolve(name)
  local triple = M._overlay[name] or PALETTE[name]
  if not triple then return nil end
  if name == "white" then
    return { r = 1, g = 1, b = 1, a = 1, space = "srgb",
             source = { fmt = "oklch", tailwind_class = name } }
  end
  if name == "black" then
    return { r = 0, g = 0, b = 0, a = 1, space = "srgb",
             source = { fmt = "oklch", tailwind_class = name } }
  end
  local L, Cval, h = triple[1], triple[2], triple[3]
  local c = C.from_oklch(L, Cval, h)
  c.source = { fmt = "oklch", tailwind_class = name }
  return c
end

-- Strip a leading utility prefix and resolve. Returns nil if the input
-- doesn't start with a known prefix.
local UTIL_PREFIXES = {
  "bg-", "text-", "border-", "ring-", "outline-", "from-", "to-", "via-",
  "decoration-", "divide-", "placeholder-", "caret-", "accent-", "fill-",
  "stroke-", "shadow-",
}

function M.resolve_class(class)
  for _, p in ipairs(UTIL_PREFIXES) do
    if class:sub(1, #p) == p then
      return M.resolve(class:sub(#p + 1))
    end
  end
  return nil
end

return M
