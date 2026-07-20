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

-- Harvest `--color-NAME: <value>;` declarations from inside @theme {} blocks
-- and update the overlay for `path`. Pure text scan; @theme bodies don't nest
-- braces. Runs on the loop (mutates module state) but is bounded by file size.
local function apply_scan(path, content)
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

-- Read a CSS file asynchronously and update the overlay from its @theme
-- blocks. The open/read run on the libuv threadpool so the disk I/O never
-- blocks the loop; the parse runs on the loop once content arrives. `cb`
-- (optional) fires once the overlay has been updated -- an unreadable file
-- still invokes it.
function M.scan_file(path, cb)
  local function finish() if cb then vim.schedule(cb) end end
  vim.uv.fs_open(path, "r", 420, function(oerr, fd)
    if oerr or not fd then return finish() end
    vim.uv.fs_fstat(fd, function(serr, stat)
      if serr or not stat then
        vim.uv.fs_close(fd, function() end)
        return finish()
      end
      vim.uv.fs_read(fd, stat.size, 0, function(rerr, data)
        vim.uv.fs_close(fd, function() end)
        vim.schedule(function()
          if not rerr and data then apply_scan(path, data) end
          if cb then cb() end
        end)
      end)
    end)
  end)
end

-- Named directories pruned from the @theme scan, on top of the walker's
-- blanket skip of hidden (dot) directories -- so only non-dot trees need
-- listing here. Not a safety measure (the walk is async and its root is
-- guarded against $HOME); purely to skip large trees that hold no theme CSS
-- (node_modules alone often holds hundreds of irrelevant CSS files).
local PRUNE = { node_modules = true, dist = true, build = true, target = true }

-- Async recursive walk: enumerate *.css files under `root` and hand the list
-- to `on_done` without ever blocking the main thread. opendir/readdir/stat all
-- run on the libuv threadpool -- so even when the walk follows a symlink into a
-- slow store (a network mount, an evicted iCloud dir whose entries fault in
-- over seconds), input and redraw stay responsive; only bookkeeping runs on
-- the loop.
--
-- Symlinks are followed (resolved with fs_stat) so projects that reach content
-- through links get scanned. Termination is guaranteed by a dev/ino visited
-- set: a real directory tree is acyclic, and any symlink loop resolves to an
-- inode already entered, so it stops; the set also dedupes a target reached
-- through several links. PRUNE and hidden directories are skipped at entry
-- (theme CSS never lives there, and they hold caches/VCS/cloud metadata not
-- worth walking). on_done fires exactly once, after the whole tree is walked.
local function walk_css_async(root, on_done)
  local out = {}
  local visited = {}   -- [dev:ino] of directories already entered
  local pending = 0
  local function done_one()
    pending = pending - 1
    if pending == 0 then vim.schedule(function() on_done(out) end) end
  end

  local scan_dir
  -- Resolve `path` through any symlink: walk it once if it's a directory,
  -- collect it if it resolves to a .css file (`is_css` from the entry name).
  local function consider(path, is_css)
    pending = pending + 1
    vim.uv.fs_stat(path, function(err, st)
      if not err and st then
        if st.type == "directory" then
          local key = st.dev .. ":" .. st.ino
          if not visited[key] then
            visited[key] = true
            scan_dir(path)
          end
        elseif st.type == "file" and is_css then
          out[#out + 1] = path
        end
      end
      done_one()
    end)
  end

  scan_dir = function(dir)
    pending = pending + 1
    vim.uv.fs_scandir(dir, function(_err, fd)
      if fd then
        while true do
          local name, t = vim.uv.fs_scandir_next(fd)
          if not name then break end
          local css = name:sub(-4) == ".css"
          if t == "file" then
            if css then out[#out + 1] = dir .. "/" .. name end
          elseif name:sub(1, 1) ~= "." and not PRUNE[name] then
            -- real subdir or symlink: resolve + recurse (cycle-guarded)
            consider(dir .. "/" .. name, css)
          end
        end
      end
      done_one()
    end)
  end

  consider(root, false)
end
-- Exposed for tests (symlink-following + cycle termination).
M._walk_css_async = walk_css_async

-- Scan a list of CSS files. Each file's read is async (libuv threadpool), so
-- the reads never block the loop; libuv's worker pool naturally throttles how
-- fast results -- and thus the small per-file parses -- arrive on the loop.
local function scan_files(files)
  for _, path in ipairs(files) do
    M.scan_file(path)
  end
end

-- Scan all *.css files under the given root (default: cwd) for @theme
-- `--color-*` declarations. Both enumeration and the per-file reads are async,
-- so nothing here blocks the UI regardless of root or storage speed:
--   1. If `rg` is on PATH, enumerate via `rg --files-with-matches` (async via
--      vim.system, respects .gitignore so node_modules etc. are skipped for
--      free, faster on large trees than the native walk).
--   2. Otherwise, or on rg error, walk the tree with walk_css_async.
function M.scan_project(root)
  root = root or vim.fn.getcwd()
  -- A @theme overlay is a per-project concept. Scanning $HOME or / pulls the
  -- walk into ~/Library cloud stores and other huge trees with no payoff, so
  -- bail unless root is a real subdirectory. Callers wanting those roots must
  -- pass them explicitly.
  local norm = vim.fs.normalize(root)
  if norm == vim.fs.normalize(vim.fn.expand("~")) or norm == "/" then return end
  if vim.fn.executable("rg") == 1 then
    -- --files-with-matches narrows to files that actually contain @theme.
    -- For projects without any @theme blocks (most), this returns nothing
    -- and we don't open a single file. exit code 1 = "no matches" (not an
    -- error), code 0 = some matches, code 2 = real error.
    vim.system(
      { "rg", "--files-with-matches", "@theme", "--type", "css" },
      { cwd = root, text = true },
      vim.schedule_wrap(function(result)
        if result.code == 0 and result.stdout and result.stdout ~= "" then
          local files = {}
          for line in result.stdout:gmatch("[^\n]+") do
            files[#files + 1] = root .. "/" .. line
          end
          scan_files(files)
        elseif result.code == 1 then
          -- No matches — overlay stays empty, we're done.
          return
        else
          -- Real error — fall back to the native async walk.
          walk_css_async(root, scan_files)
        end
      end)
    )
    return
  end
  walk_css_async(root, scan_files)
end

return M
