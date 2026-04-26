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

-- Directories pruned from the project @theme scan. Walking these can dominate
-- startup in monorepos (node_modules alone often holds hundreds of CSS files
-- nobody cares about for theme resolution).
local PRUNE = { node_modules = true, [".git"] = true, dist = true, build = true,
                [".next"] = true, ["target"] = true, [".venv"] = true }

-- Recursive directory walk that prunes the PRUNE set at directory entry,
-- avoiding the cost of fnmatch on already-walked paths. Returns CSS file
-- paths under root.
local function walk_css(root, out)
  out = out or {}
  local fd = vim.uv.fs_scandir(root)
  if not fd then return out end
  while true do
    local name, t = vim.uv.fs_scandir_next(fd)
    if not name then break end
    if t == "directory" then
      if not PRUNE[name] then
        walk_css(root .. "/" .. name, out)
      end
    elseif t == "file" and name:sub(-4) == ".css" then
      out[#out + 1] = root .. "/" .. name
    end
  end
  return out
end

-- How much wall time we let scan_project occupy the main thread per chunk.
-- 1ms is below human perception even for 144Hz displays (6.9ms frame budget),
-- and we yield via vim.defer_fn(0) between chunks so input/render is never
-- blocked.
local CHUNK_BUDGET_MS = 1

-- Iterate `files` in cooperative chunks; never blocks the main thread for
-- more than CHUNK_BUDGET_MS at a time.
local function scan_files_chunked(files)
  local i = 1
  local function chunk()
    local start = vim.uv.hrtime()
    while i <= #files do
      pcall(M.scan_file, files[i])
      i = i + 1
      if (vim.uv.hrtime() - start) / 1e6 >= CHUNK_BUDGET_MS then break end
    end
    if i <= #files then
      vim.defer_fn(chunk, 0)
    end
  end
  vim.defer_fn(chunk, 0)
end

-- Scan all *.css/*.scss files under the given root (default: cwd). Two
-- progressive enhancements:
--   1. If `rg` is on PATH, use `rg --files --type css` to enumerate files —
--      async via vim.system, respects .gitignore (so node_modules is auto-
--      skipped without our explicit prune list), and faster on large trees
--      than the native walk. Includes .scss too via rg's built-in type.
--   2. Otherwise fall back to the native vim.uv walk with directory pruning.
-- Both paths feed the same cooperative chunked file processor.
-- `sync = true` forces the synchronous walk + processing (used by tests).
function M.scan_project(root, sync)
  root = root or vim.fn.getcwd()
  if sync then
    for _, f in ipairs(walk_css(root)) do pcall(M.scan_file, f) end
    return
  end
  if vim.fn.executable("rg") == 1 then
    vim.system(
      { "rg", "--files", "--type", "css" },
      { cwd = root, text = true },
      vim.schedule_wrap(function(result)
        if result.code ~= 0 or not result.stdout then
          -- rg failed; fall back to native walk
          scan_files_chunked(walk_css(root))
          return
        end
        local files = {}
        for line in result.stdout:gmatch("[^\n]+") do
          files[#files + 1] = root .. "/" .. line
        end
        scan_files_chunked(files)
      end)
    )
    return
  end
  scan_files_chunked(walk_css(root))
end

return M
