-- lua/lib/colors/tailwind.lua
-- Tailwind v4 palette resolution: built-in defaults + project @theme overlay.
local C       = require("lib.colors.color")
local PALETTE = require("lib.colors._tailwind_palette")
local M       = {}

-- Project overlay: `--color-*` declarations harvested from project CSS files.
-- Populated by scan_file/scan_project (Task 12). Same { L, C, h } shape as
-- the built-in palette.
M._overlay = {}

-- Per-file index of names written to _overlay: { [path] = { name, ... } }
M._overlay_by_file = {}

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

-- Lazy require to avoid a require cycle (tailwind <-> parse).
local _parse, _color
local function parse() if not _parse then _parse = require("lib.colors.parse") end; return _parse end
local function color() if not _color then _color = require("lib.colors.color") end; return _color end

-- Read a CSS file and harvest `--color-NAME: <value>;` declarations from
-- inside @theme {} blocks. Pure text scan; @theme bodies don't nest braces.
function M.scan_file(path)
  local fd = io.open(path, "r")
  if not fd then return end
  local content = fd:read("*a")
  fd:close()

  -- Harvest the new names + their values for this file
  local new_keys = {}
  for body in content:gmatch("@theme%s*{(.-)}") do
    for name, value in body:gmatch("%-%-color%-([%w%-]+)%s*:%s*([^;]+);") do
      local results = parse().parse_all(value)
      if #results > 0 then
        local L, Cval, h = color().to_oklch(results[1].color)
        new_keys[name] = { L, Cval, h }
      end
    end
  end

  -- Drop any keys from this file's previous scan that aren't in the new set
  for _, name in ipairs(M._overlay_by_file[path] or {}) do
    if not new_keys[name] then M._overlay[name] = nil end
  end

  -- Apply the new set + record the keys we own from this file
  local owned = {}
  for name, triple in pairs(new_keys) do
    M._overlay[name] = triple
    table.insert(owned, name)
  end
  M._overlay_by_file[path] = owned
end

-- Scan all *.css files under the given root (default: cwd). Skips common
-- vendored / build directories.
function M.scan_project(root)
  root = root or vim.fn.getcwd()
  local files = vim.fn.globpath(root, "**/*.css", false, true)
  for _, f in ipairs(files) do
    if not (f:match("/node_modules/") or f:match("/%.git/")
            or f:match("/dist/") or f:match("/build/")) then
      pcall(M.scan_file, f)
    end
  end
end

return M
