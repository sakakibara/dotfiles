--
-- Two-zone winbar:
--   LEFT   — code scope (treesitter symbol trail, primary focus)
--   RIGHT  — file path (secondary context, dimmed)
--
-- Both zones are clickable. Each scope segment opens a sibling-symbol
-- picker; each path segment opens a dir listing picker (with l/<Right>
-- drill-down into subdirectories, provided by lib.winbar_menu).
--
-- Symbol trail is cached on (changedtick, cursor_row, cursor_col) so
-- redraws from non-cursor events (BufEnter, WinEnter, ColorScheme) don't
-- redo the treesitter walk.
--
-- Keyboard entrypoints on config/keymaps.lua:
--   <Leader>;   → M.pick_scope()   sibling-symbol picker at cursor
--   <Leader>.   → M.pick_path()    directory picker for current file
--
-- Inactive windows get path-only (scope depends on cursor, which lives
-- in the active window).

local M = {}
local menu = require("lib.winbar_menu")

-- Breadcrumb separator (padded Nerd-Font chevron), resolved lazily so
-- Lib isn't forced at require time.
local function SEP() return " " .. Lib.icons.status.Separator.Breadcrumb .. " " end

-- Get the path-like name for a buffer, resolving oil:// URIs to their
-- actual directory so the winbar path zone shows real segments.
local function buf_path_name(buf)
  if vim.bo[buf].filetype == "oil" then
    local ok, oil = pcall(require, "oil")
    if ok and oil.get_current_dir then
      return oil.get_current_dir(buf) or ""
    end
  end
  return vim.api.nvim_buf_get_name(buf)
end

-- ─────────────────────────────────────────────────────────────────────
-- ICON MAP — keys match Lib.icons.kinds entries. Resolved lazily so we
-- don't force Lib load at module require.
-- ─────────────────────────────────────────────────────────────────────
local function kind_icon(kind)
  local map = {
    class     = "Class",     struct    = "Struct",     interface = "Interface",
    enum      = "Enum",      enum_member = "EnumMember",
    function_ = "Function",  method    = "Method",
    module    = "Module",    namespace = "Namespace",
    type      = "TypeParameter",
    impl      = "Class",     trait     = "Interface",
  }
  local key = map[kind]
  return key and Lib.icons.kinds[key] or ""
end

-- ─────────────────────────────────────────────────────────────────────
-- TS NODE TYPE TABLE — maps treesitter node type → { kind = <logical> }
-- `kind` feeds kind_icon() for display, and also becomes a grouping
-- label for the sibling picker.
-- ─────────────────────────────────────────────────────────────────────
local T = {
  lua = {
    function_declaration    = "function_",
    function_definition     = "function_",
    local_function          = "function_",
    method_index_expression = "method",
  },
  python = {
    function_definition = "function_",
    class_definition    = "class",
  },
  typescript = {
    function_declaration  = "function_",
    method_definition     = "method",
    class_declaration     = "class",
    arrow_function        = "function_",
    interface_declaration = "interface",
    enum_declaration      = "enum",
    type_alias_declaration= "type",
  },
  typescriptreact = {
    function_declaration  = "function_",
    method_definition     = "method",
    class_declaration     = "class",
    arrow_function        = "function_",
    interface_declaration = "interface",
  },
  javascript = {
    function_declaration = "function_",
    method_definition    = "method",
    class_declaration    = "class",
    arrow_function       = "function_",
  },
  javascriptreact = {
    function_declaration = "function_",
    method_definition    = "method",
    class_declaration    = "class",
    arrow_function       = "function_",
  },
  rust = {
    function_item = "function_",
    impl_item     = "impl",
    trait_item    = "trait",
    struct_item   = "struct",
    enum_item     = "enum",
    mod_item      = "module",
  },
  go = {
    function_declaration = "function_",
    method_declaration   = "method",
    type_declaration     = "type",
  },
  c = {
    function_definition = "function_",
    struct_specifier    = "struct",
    enum_specifier      = "enum",
  },
  cpp = {
    function_definition = "function_",
    class_specifier     = "class",
    struct_specifier    = "struct",
    enum_specifier      = "enum",
    namespace_definition= "namespace",
  },
  java = {
    method_declaration    = "method",
    class_declaration     = "class",
    interface_declaration = "interface",
    enum_declaration      = "enum",
  },
  ruby = {
    method                = "method",
    singleton_method      = "method",
    class                 = "class",
    module                = "module",
  },
  zig = {
    function_declaration = "function_",
    struct_declaration   = "struct",
    enum_declaration     = "enum",
  },
}

local function types_for(ft) return T[ft] end

-- ─────────────────────────────────────────────────────────────────────
-- CLICK DISPATCHER — winbar `%@` callbacks look up actions by id.
-- Each handler carries its rendered screen column so menu.open can
-- anchor under the clicked segment.
-- ─────────────────────────────────────────────────────────────────────
local handlers, next_id = {}, 100

local function register(action)
  local id = next_id
  next_id = next_id + 1
  if next_id > 9999 then next_id = 100 end
  handlers[id] = { action = action, col = 0 }
  return id
end

function M.click(minwid, _, button, _)
  local h = handlers[minwid]
  if not h then return end
  -- Winbar clicks don't move focus, so any menu that's already open
  -- stays open unless we close it here. Without this, clicking a
  -- segment while a menu is up would stack a new menu on top.
  menu.close_all()
  -- Capture the real source window via mouse position; current_win
  -- would point at the menu float we just closed.
  local pos = vim.fn.getmousepos()
  local src_win = (pos.winid ~= 0 and vim.api.nvim_win_is_valid(pos.winid))
    and pos.winid or nil
  h.action(button, h.col, src_win)
end

local function clickable(id, content)
  return ("%%%d@v:lua.Lib.winbar.click@%s%%X"):format(id, content)
end

-- ─────────────────────────────────────────────────────────────────────
-- TREESITTER WALKS
-- ─────────────────────────────────────────────────────────────────────
local function get_symbol_name(node, bufnr)
  -- Most treesitter grammars expose the symbol name via a "name" field.
  -- Try that first — it handles Lua's `M.outer` (dot_index_expression),
  -- Rust's scoped names, TypeScript's computed method names, etc.
  local fields = node:field("name")
  if fields and fields[1] then
    return vim.treesitter.get_node_text(fields[1], bufnr)
  end
  -- Fallback: scan named children for anything name-like.
  for i = 0, node:named_child_count() - 1 do
    local child = node:named_child(i)
    if child then
      local t = child:type()
      if t == "identifier" or t:find("identifier") or t:find("name") or t == "dot_index_expression" then
        return vim.treesitter.get_node_text(child, bufnr)
      end
    end
  end
  return nil
end

local function root_node(bufnr, ft)
  local ok, parser = pcall(vim.treesitter.get_parser, bufnr, ft)
  if not ok or not parser then return nil end
  parser:parse()
  local tree = parser:trees()[1]
  return tree and tree:root() or nil
end

local function symbol_trail_raw(bufnr, row, col)
  local ft    = vim.bo[bufnr].filetype
  local types = types_for(ft)
  if not types then return {}, nil end
  local root = root_node(bufnr, ft)
  if not root then return {}, types end

  local node  = root:named_descendant_for_range(row, col, row, col)
  local trail = {}
  while node do
    local kind = types[node:type()]
    if kind then
      local name = get_symbol_name(node, bufnr)
      if name then
        table.insert(trail, 1, { node = node, name = name, kind = kind })
      end
    end
    node = node:parent()
  end
  return trail, types
end

-- Cache: { [bufnr] = { tick, row, col, trail, types } }
local scope_cache = {}
local function symbol_trail(bufnr, row, col)
  local entry = scope_cache[bufnr]
  local tick  = vim.api.nvim_buf_get_changedtick(bufnr)
  if entry and entry.tick == tick and entry.row == row and entry.col == col then
    return entry.trail, entry.types
  end
  local trail, types = symbol_trail_raw(bufnr, row, col)
  scope_cache[bufnr] = { tick = tick, row = row, col = col, trail = trail, types = types }
  return trail, types
end

-- Sibling symbols of a node at the same parent scope.
local function sibling_symbols(bufnr, node, types)
  local parent = node:parent() or node
  local self_sr = ({ node:range() })[1]
  local out = {}
  for child in parent:iter_children() do
    local kind = types[child:type()]
    if kind then
      local sname = get_symbol_name(child, bufnr)
      if sname then
        local csr, csc = child:range()
        out[#out + 1] = {
          label   = sname,
          icon    = kind_icon(kind),
          lnum    = csr + 1,
          col     = csc + 1,
          current = csr == self_sr,
        }
      end
    end
  end
  return out
end

-- ─────────────────────────────────────────────────────────────────────
-- FILESYSTEM HELPERS
-- ─────────────────────────────────────────────────────────────────────
local function list_dir(path)
  local fd = vim.uv.fs_scandir(path)
  if not fd then return {} end
  local entries = {}
  while true do
    local name, ftype = vim.uv.fs_scandir_next(fd)
    if not name then break end
    if not name:match("^%.") then
      entries[#entries + 1] = { name = name, type = ftype }
    end
  end
  table.sort(entries, function(a, b)
    if a.type ~= b.type then return a.type == "directory" end
    return a.name:lower() < b.name:lower()
  end)
  return entries
end

local function dir_items(path)
  local items = {}
  for _, e in ipairs(list_dir(path)) do
    local full = path .. "/" .. e.name
    items[#items + 1] = {
      label  = e.name .. (e.type == "directory" and "/" or ""),
      icon   = Lib.icons.status.Directory and (e.type == "directory" and Lib.icons.status.Directory or "") or "",
      file   = full,
      is_dir = e.type == "directory",
    }
    if e.type ~= "directory" then
      local ok, devicons = pcall(require, "nvim-web-devicons")
      if ok then
        local ext = e.name:match("%.([^%.]+)$")
        local icon = devicons.get_icon(e.name, ext, { default = true })
        items[#items].icon = icon and (icon .. " ") or " "
      end
    end
  end
  return items
end

-- ─────────────────────────────────────────────────────────────────────
-- SEGMENTS
-- ─────────────────────────────────────────────────────────────────────
-- Each segment builder returns { content, width, id } so render() can
-- track the rendered screen column and patch handlers[id].col.

local function segment_scope(bufnr, entry, types)
  local id = register(function(_, col, src_win)
    local siblings = sibling_symbols(bufnr, entry.node, types)
    menu.open(siblings, {
      title        = entry.kind or "symbols",
      anchor_col   = col,
      original_win = src_win,
      on_pick      = function(item)
        vim.api.nvim_win_set_cursor(0, { item.lnum, item.col - 1 })
        vim.cmd("normal! zz")
      end,
    })
  end)
  local text = kind_icon(entry.kind) .. entry.name
  return {
    content = clickable(id, "%#WinBarScope#" .. text),
    width   = vim.fn.strdisplaywidth(text),
    id      = id,
  }
end

local function segment_path_dir(abs_path, label)
  local id = register(function(_, col, src_win)
    local items = dir_items(abs_path)
    if #items == 0 then return end
    menu.open(items, {
      title        = label,
      anchor_col   = col,
      original_win = src_win,
      drill        = function(item) return item.is_dir and dir_items(item.file) or nil end,
      on_pick      = function(item)
        if item.is_dir then
          vim.cmd.cd(item.file)
          vim.notify("`" .. vim.fn.fnamemodify(item.file, ":~") .. "`", vim.log.levels.INFO, { title = "cd" })
        else vim.cmd.edit(vim.fn.fnameescape(item.file)) end
      end,
    })
  end)
  return {
    content = clickable(id, "%#WinBarPath#" .. label),
    width   = vim.fn.strdisplaywidth(label),
    id      = id,
  }
end

local function segment_path_file(abs_path, label, modified)
  local parent = vim.fn.fnamemodify(abs_path, ":h")
  local id = register(function(_, col, src_win)
    local items = dir_items(parent)
    if #items == 0 then return end
    for _, item in ipairs(items) do
      item.current = (item.file == abs_path)
    end
    menu.open(items, {
      title        = vim.fn.fnamemodify(parent, ":t"),
      anchor_col   = col,
      original_win = src_win,
      drill        = function(item) return item.is_dir and dir_items(item.file) or nil end,
      on_pick      = function(item)
        if item.is_dir then vim.cmd.cd(item.file)
        else vim.cmd.edit(vim.fn.fnameescape(item.file)) end
      end,
    })
  end)
  local icon = ""
  if vim.fn.isdirectory(abs_path) == 1 then
    icon = Lib.icons.status.Directory
  else
    local ok, devicons = pcall(require, "nvim-web-devicons")
    if ok then
      local ext = vim.fn.fnamemodify(abs_path, ":e")
      local ic = devicons.get_icon(abs_path, ext, { default = true })
      if ic then icon = ic .. " " end
    end
  end
  local mod = modified and " [+]" or ""
  local text = icon .. label .. mod
  return {
    content = clickable(id, "%#WinBarFile#" .. text),
    width   = vim.fn.strdisplaywidth(text),
    id      = id,
  }
end

-- ─────────────────────────────────────────────────────────────────────
-- RENDER
-- ─────────────────────────────────────────────────────────────────────
function M.render()
  local winid      = vim.g.statusline_winid or vim.api.nvim_get_current_win()
  local bufnr      = vim.api.nvim_win_get_buf(winid)
  local is_current = winid == vim.api.nvim_get_current_win()

  -- Skip winbar for special buffers
  local bt = vim.bo[bufnr].buftype
  if bt == "nofile" or bt == "prompt" or bt == "terminal" or bt == "quickfix" then
    return ""
  end

  local name = buf_path_name(bufnr)
  if name == "" then
    return "%#WinBarDim#[No Name]"
  end

  -- Bound memory growth on the handlers map.
  if next_id > 500 then handlers, next_id = {}, 100 end

  -- Each zone is built as a list of { content, width, id? }. After
  -- concatenation, we walk them to patch handlers[id].col with the
  -- segment's screen column so menus anchor under the clicked spot.
  local sep = SEP()
  local sep_item = { content = "%#WinBarDim#" .. sep, width = vim.fn.strdisplaywidth(sep) }

  -- ── LEFT: scope breadcrumb (active window only) ─────────────────────
  local scope_out = {}
  if is_current then
    local cur = vim.api.nvim_win_get_cursor(winid)
    local trail, types = symbol_trail(bufnr, cur[1] - 1, cur[2])
    if trail and types and #trail > 0 then
      for i, entry in ipairs(trail) do
        if i > 1 then scope_out[#scope_out + 1] = sep_item end
        scope_out[#scope_out + 1] = segment_scope(bufnr, entry, types)
      end
    end
  end

  -- ── RIGHT: dim path (always) ────────────────────────────────────────
  -- Strip oil's / other directory-buffer trailing slash before splitting
  -- so we don't get an empty last segment.
  local rel     = vim.fn.fnamemodify(name, ":~:."):gsub("/$", "")
  local abs     = vim.fn.fnamemodify(name, ":p"):gsub("/$", "")
  local parts   = vim.split(rel, "/", { plain = true })
  local path_out = {}
  local walked  = rel:sub(1, 1) == "~" and vim.fn.expand("~") or ""

  for i, p in ipairs(parts) do
    if i == 1 and p == "~" then
      walked = vim.fn.expand("~")
    elseif i < #parts then
      walked = walked == "" and p or (walked .. "/" .. p)
      path_out[#path_out + 1] = segment_path_dir(walked, p)
      path_out[#path_out + 1] = sep_item
    else
      path_out[#path_out + 1] = segment_path_file(abs, p, vim.bo[bufnr].modified)
    end
  end

  -- ── Adaptive trim: drop leading dir segments until the path zone fits
  --    the width left over by scope + padding. Replace dropped prefix
  --    with a dim ellipsis so the truncation is visible.
  local win_pos   = vim.api.nvim_win_get_position(winid)
  local win_col   = win_pos[2] or 0
  local win_width = vim.api.nvim_win_get_width(winid)

  local function sum_width(list)
    local t = 0
    for _, it in ipairs(list) do t = t + it.width end
    return t
  end

  local scope_w = sum_width(scope_out)
  local budget  = win_width - scope_w - 2  -- small margin

  if #path_out > 1 and sum_width(path_out) > budget then
    local ell = "…"
    local ellipsis_item = { content = "%#WinBarDim#" .. ell .. sep,
                            width = vim.fn.strdisplaywidth(ell) + sep_item.width }
    -- Drop leading dir+sep pairs until we fit, or only the file remains.
    while #path_out > 1 and sum_width(path_out) + ellipsis_item.width > budget do
      -- Path structure: [dir sep dir sep ... dir sep file]. Remove the
      -- first TWO items (dir + trailing sep) each iteration.
      table.remove(path_out, 1)
      if path_out[1] and path_out[1] == sep_item then
        table.remove(path_out, 1)
      end
    end
    table.insert(path_out, 1, ellipsis_item)
  end

  -- Scope grows left-to-right from the window's left edge
  do
    local col = win_col
    for _, item in ipairs(scope_out) do
      if item.id then handlers[item.id].col = col end
      col = col + item.width
    end
  end

  -- Path is right-aligned via %=, so position from the right edge inward
  do
    local col = win_col + win_width - sum_width(path_out)
    for _, item in ipairs(path_out) do
      if item.id then handlers[item.id].col = col end
      col = col + item.width
    end
  end

  local left  = table.concat(vim.tbl_map(function(it) return it.content end, scope_out))
  local right = table.concat(vim.tbl_map(function(it) return it.content end, path_out))
  return left .. "%#WinBarFill#%=" .. right
end

-- ─────────────────────────────────────────────────────────────────────
-- KEYBOARD ENTRYPOINTS
-- ─────────────────────────────────────────────────────────────────────
function M.pick_scope()
  local winid = vim.api.nvim_get_current_win()
  local bufnr = vim.api.nvim_win_get_buf(winid)
  local cur   = vim.api.nvim_win_get_cursor(winid)
  local trail, types = symbol_trail(bufnr, cur[1] - 1, cur[2])
  if not trail or #trail == 0 or not types then
    vim.notify("no scope at cursor", vim.log.levels.INFO); return
  end
  local entry    = trail[#trail]  -- innermost
  local siblings = sibling_symbols(bufnr, entry.node, types)
  menu.open(siblings, {
    title = entry.kind or "symbols",
    on_pick = function(item)
      vim.api.nvim_win_set_cursor(0, { item.lnum, item.col - 1 })
      vim.cmd("normal! zz")
    end,
  })
end

function M.pick_path()
  local bufnr = vim.api.nvim_get_current_buf()
  local name  = vim.api.nvim_buf_get_name(bufnr)
  if name == "" then return end
  local abs    = vim.fn.fnamemodify(name, ":p")
  local parent = vim.fn.fnamemodify(abs,  ":h")
  local items  = dir_items(parent)
  if #items == 0 then return end
  for _, item in ipairs(items) do item.current = (item.file == abs) end
  menu.open(items, {
    title = vim.fn.fnamemodify(parent, ":t"),
    drill = function(item) return item.is_dir and dir_items(item.file) or nil end,
    on_pick = function(item)
      if item.is_dir then vim.cmd.cd(item.file)
      else vim.cmd.edit(vim.fn.fnameescape(item.file)) end
    end,
  })
end

-- Back-compat: old keymap bound Lib.winbar.pick() → map to file picker.
M.pick = M.pick_path

-- ─────────────────────────────────────────────────────────────────────
-- SETUP
-- ─────────────────────────────────────────────────────────────────────
function M.setup()
  local function hex(n, a)
    local h = vim.api.nvim_get_hl(0, { name = n, link = false })
    local v = h[a]
    return type(v) == "number" and string.format("#%06x", v) or nil
  end

  local function apply_hl()
    local scope_fg = hex("Function",  "fg") or hex("Normal",   "fg")
    local file_fg  = hex("Directory", "fg") or hex("Special",  "fg") or scope_fg
    local dim_fg   = hex("Comment",   "fg") or hex("NonText",  "fg")
    local path_fg  = dim_fg
    local bar_bg = hex("StatusLine", "bg") or hex("Pmenu", "bg") or hex("NormalFloat", "bg")
    vim.api.nvim_set_hl(0, "WinBarScope", { fg = scope_fg, bg = bar_bg, bold = true })
    vim.api.nvim_set_hl(0, "WinBarFile",  { fg = file_fg,  bg = bar_bg, bold = true })
    vim.api.nvim_set_hl(0, "WinBarPath",  { fg = path_fg,  bg = bar_bg, italic = true })
    vim.api.nvim_set_hl(0, "WinBarDim",   { fg = dim_fg,   bg = bar_bg, italic = true })
    vim.api.nvim_set_hl(0, "WinBarFill",  { fg = dim_fg,   bg = bar_bg })
    vim.api.nvim_set_hl(0, "WinBar",      { fg = hex("Normal", "fg"), bg = bar_bg })
    vim.api.nvim_set_hl(0, "WinBarNC",    { fg = dim_fg, bg = bar_bg, italic = true })
  end
  apply_hl()

  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("Lib.winbar.hl", { clear = true }),
    callback = apply_hl,
  })

  -- Invalidate cache when buffer is unloaded/wiped
  vim.api.nvim_create_autocmd({ "BufWipeout", "BufUnload" }, {
    group = vim.api.nvim_create_augroup("Lib.winbar.cache", { clear = true }),
    callback = function(args) scope_cache[args.buf] = nil end,
  })

  -- Disable the winbar entirely (no line allocated) for special-purpose
  -- windows. The global `winbar = "%!Lib.winbar.render()"` would otherwise
  -- leave a blank line at the top of these windows: render() returns ""
  -- for special buftypes, but the option itself is still non-empty, so
  -- nvim allocates the bar and just paints empty content. Setting the
  -- window-local option to "" disables the bar for that window outright.
  --
  -- "" via nvim_set_option_value with scope=local makes the local empty;
  -- to inherit the global expression for normal windows, we never touch
  -- their winbar (it stays unset = inherits global).
  vim.api.nvim_create_autocmd({ "BufWinEnter", "TermOpen" }, {
    group = vim.api.nvim_create_augroup("Lib.winbar.disable_special", { clear = true }),
    callback = function(args)
      local win = vim.api.nvim_get_current_win()
      if vim.api.nvim_win_get_buf(win) ~= args.buf then return end
      -- Buffer-local opt-out: a creator that wants to render its own
      -- winbar on a special buffer sets b:lib_winbar_keep before this
      -- autocmd has a chance to clobber the local winbar option.
      if vim.b[args.buf].lib_winbar_keep then return end
      local bt = vim.bo[args.buf].buftype
      if bt == "nofile" or bt == "prompt" or bt == "terminal" or bt == "quickfix" then
        vim.wo[win].winbar = ""
      end
    end,
  })

  -- Redraw on cursor move (scope updates live) or window resize (the
  -- adaptive path trim needs to re-evaluate budget). Symbol-trail cache
  -- short-circuits treesitter work when nothing relevant changed.
  vim.api.nvim_create_autocmd({
    "CursorMoved", "CursorMovedI", "BufEnter", "WinEnter",
    "WinResized", "VimResized",
  }, {
    group = vim.api.nvim_create_augroup("Lib.winbar.redraw", { clear = true }),
    callback = function()
      pcall(vim.api.nvim__redraw, { winbar = true })
    end,
  })
end

return M
