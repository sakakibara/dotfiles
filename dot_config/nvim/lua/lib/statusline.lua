-- lua/lib/statusline.lua
--
-- Statusline designed for two things:
--   1. Premium appearance: powerline-slant separators, catppuccin-sourced
--      palette, clear typographic hierarchy (mode block > primary > dim),
--      breathing mode color animation.
--   2. Snappiness: per-buffer per-segment cache, invalidated only on
--      relevant events. Hot render path is pure table.concat of cached
--      strings — no IO, no LSP/diagnostic scans, no path manipulation.
--
-- No backwards compat with heirline or lualine. This is ours.

local M = {}

-- ============================================================================
-- PALETTE + HIGHLIGHTS
-- ============================================================================

-- Derive palette from CURRENT colorscheme's standard highlight groups.
-- Works with any theme (catppuccin, tokyonight, gruvbox, kanagawa, etc.)
-- because we reference semantic groups every colorscheme defines.

local palette  -- filled by derive_palette()

local function hl(name, opts)
  vim.api.nvim_set_hl(0, name, opts)
end

local function get(name, attr, fallback)
  local h = vim.api.nvim_get_hl(0, { name = name, link = false })
  local v = h[attr]
  if type(v) == "number" then return string.format("#%06x", v) end
  return fallback
end

-- Small helper to compute a slightly-different bg tint for group alternation.
local function shade(hex, delta)
  if not hex or hex == "NONE" then return hex end
  local r = tonumber(hex:sub(2,3), 16)
  local g = tonumber(hex:sub(4,5), 16)
  local b = tonumber(hex:sub(6,7), 16)
  r = math.max(0, math.min(255, r + delta))
  g = math.max(0, math.min(255, g + delta))
  b = math.max(0, math.min(255, b + delta))
  return string.format("#%02x%02x%02x", r, g, b)
end

local function derive_palette()
  -- Backgrounds: use Folded.bg for the raised statusline pane (lighter than
  -- window bg, matches heirline/lualine convention and gives visual lift).
  local bg_end   = get("Normal", "bg", "#000000")
  local bg_a     = get("Folded", "bg", nil) or get("PmenuSel", "bg", nil) or get("Visual", "bg", bg_end)
  -- Slightly different tint for alternating groups (lighter by 12 rgb units)
  local bg_b     = shade(bg_a, 12)
  -- Main fg comes from Normal, not Folded (Folded.fg is the fold arrow color)
  local fg       = get("StatusLine", "fg", nil) or get("Normal", "fg", "#ffffff")
  local fg_bold  = get("Normal", "fg", "#ffffff")
  local fg_dim   = get("NonText", "fg", nil) or get("Comment", "fg", fg)

  -- Semantic source groups — every theme defines these
  local red      = get("DiagnosticError", "fg", nil) or get("Error",       "fg", "#ff5555")
  local yellow   = get("DiagnosticWarn",  "fg", nil) or get("WarningMsg",  "fg", "#f1fa8c")
  local info     = get("DiagnosticInfo",  "fg", nil) or get("Function",    "fg", "#8be9fd")
  local hint     = get("DiagnosticHint",  "fg", nil) or get("Special",     "fg", "#50fa7b")
  local green    = get("String",          "fg", nil) or "#50fa7b"
  local blue     = get("Function",        "fg", nil) or "#8be9fd"
  local purple   = get("Statement",       "fg", nil) or get("Keyword",     "fg", "#bd93f9")
  local orange   = get("Constant",        "fg", nil) or get("Number",      "fg", "#ffb86c")
  local cyan     = get("Special",         "fg", nil) or get("Type",        "fg", "#8be9fd")
  local git_add  = get("diffAdded",       "fg", nil) or green
  local git_del  = get("diffRemoved",     "fg", nil) or get("diffDeleted", "fg", red)
  local git_chg  = get("diffChanged",     "fg", nil) or yellow

  palette = {
    bg_end = bg_end, bg_mid = bg_a, fg = fg, fg_bold = fg_bold, fg_dim = fg_dim,
    red = red, yellow = yellow, green = green, blue = blue,
    purple = purple, orange = orange, cyan = cyan,
    info = info, hint = hint,
    git_add = git_add, git_del = git_del, git_chg = git_chg,
  }
end

local function define_highlights()
  derive_palette()
  local p = palette

  hl("StslNc",        { fg = p.fg_dim, bg = p.bg_end })
  hl("Stsl",          { fg = p.fg, bg = p.bg_mid })
  hl("StslDim",       { fg = p.fg_dim, bg = p.bg_mid })
  hl("StslBold",      { fg = p.fg, bg = p.bg_mid, bold = true })

  -- Mode block (fg/bg + caps driven by lib/mode_color on ModeChanged)
  hl("StslMode",      { fg = p.bg_end, bg = p.red, bold = true })
  hl("StslModeSep",   { fg = p.red, bg = p.bg_mid })
  hl("StslCapL",      { fg = p.red, bg = p.bg_end })
  hl("StslCapR",      { fg = p.bg_mid, bg = p.bg_end })

  hl("StslGit",       { fg = p.purple,   bg = p.bg_mid })
  hl("StslGitAdd",    { fg = p.git_add,  bg = p.bg_mid })
  hl("StslGitChange", { fg = p.git_chg,  bg = p.bg_mid })
  hl("StslGitDel",    { fg = p.git_del,  bg = p.bg_mid })
  hl("StslMod",       { fg = p.orange,   bg = p.bg_mid, bold = true })
  hl("StslRO",        { fg = p.red,      bg = p.bg_mid })
  hl("StslSpell",     { fg = p.green,    bg = p.bg_mid })
  hl("StslSnip",      { fg = p.red,      bg = p.bg_mid, bold = true })
  hl("StslSearch",    { fg = p.purple,   bg = p.bg_mid, bold = true })
  hl("StslMacro",     { fg = p.red,      bg = p.bg_mid, bold = true })
  hl("StslDiagErr",   { fg = p.red,      bg = p.bg_mid })
  hl("StslDiagWarn",  { fg = p.yellow,   bg = p.bg_mid })
  hl("StslDiagInfo",  { fg = p.info,     bg = p.bg_mid })
  hl("StslDiagHint",  { fg = p.hint,     bg = p.bg_mid })
  hl("StslLsp",       { fg = p.green,    bg = p.bg_mid, bold = true })
  hl("StslFT",        { fg = p.fg_bold,  bg = p.bg_mid, bold = true })

  -- File-path zones (cwd / relative-dir / basename), mirroring the old
  -- heirline split: dim gray cwd, themed-accent relative dir, bright
  -- bold basename (orange when modified, red when readonly).
  hl("StslPathCwd",   { fg = p.fg_dim,   bg = p.bg_mid, italic = true })
  hl("StslPathRel",   { fg = p.blue,     bg = p.bg_mid })
  hl("StslPathFile",  { fg = p.fg_bold,  bg = p.bg_mid, bold = true })
  hl("StslPathMod",   { fg = p.orange,   bg = p.bg_mid, bold = true, italic = true })
  hl("StslPathRO",    { fg = p.red,      bg = p.bg_mid, bold = true })
  hl("StslEnc",       { fg = p.yellow,   bg = p.bg_mid })
  hl("StslBom",       { fg = p.red,      bg = p.bg_mid, bold = true })
  hl("StslFmt",       { fg = p.yellow,   bg = p.bg_mid })
  hl("StslAFOff",     { fg = p.yellow,   bg = p.bg_mid, bold = true })
  hl("StslAFOn",      { fg = p.green,    bg = p.bg_mid, bold = true })
  hl("StslDap",       { fg = p.orange,   bg = p.bg_mid, bold = true })
  hl("StslScroll",    { fg = p.blue,     bg = p.bg_mid })
  hl("StslPos",       { fg = p.fg_bold,  bg = p.bg_mid, bold = true })
end

-- ============================================================================
-- POWERLINE SYMBOLS
-- ============================================================================

local SEP = {
  cap_l    = "",  -- U+E0B6 rounded left
  cap_r    = "",  -- U+E0B4 rounded right
  slant_r  = "",  -- U+E0B8 slant-right (mode→mid)
  slant_l  = "",  -- U+E0BA slant-left  (mid→mode on right side, unused)
}

-- 6 levels of horizontal-bar glyphs (U+1FB76..U+1FB7B). Used as a 2-char
-- scrollbar: picks one glyph for the cursor's vertical position in the
-- file and renders it twice. Matches old heirline scrollbar.
local BAR_BLOCKS = { "🭶", "🭷", "🭸", "🭹", "🭺", "🭻" }

-- ============================================================================
-- PUBLIC SEGMENT BUILDERS (for tests)
-- ============================================================================

local MODE_NAMES = {
  n = "NORMAL", no = "O-PEND", niI = "NORMAL", niR = "NORMAL", niV = "NORMAL",
  i = "INSERT", ic = "INSERT", ix = "INSERT",
  v = "VISUAL", vs = "VISUAL", V = "V-LINE", Vs = "V-LINE",
  ["\22"] = "V-BLOCK", ["\22s"] = "V-BLOCK",
  s = "SELECT", S = "S-LINE", ["\19"] = "S-BLOCK",
  R = "REPLACE", Rc = "REPLACE", Rx = "REPLACE", Rv = "V-RPLCE",
  c = "COMMAND", cv = "EX",
  r = "PROMPT", rm = "MORE", ["r?"] = "CONFIRM",
  ["!"] = "SHELL", t = "TERMINAL",
}

M._segments = {}

function M._segments.mode(mode)
  mode = mode or vim.fn.mode()
  return "%#StslCapL#" .. SEP.cap_l .. "%#StslMode# " .. (MODE_NAMES[mode] or "??") .. " %#StslModeSep#" .. SEP.slant_r
end

function M._segments._short_path(path, maxlen)
  if #path <= maxlen then return path end
  local parts = vim.split(path, "/", { plain = true })
  if #parts <= 2 then return path end
  return parts[1] .. "/…/" .. parts[#parts]
end

function M._segments.scrollbar(lnum, total)
  if total <= 1 then return BAR_BLOCKS[1] .. BAR_BLOCKS[1] end
  local i = math.floor((lnum - 1) / total * (#BAR_BLOCKS - 1)) + 1
  return string.rep(BAR_BLOCKS[i], 2)
end

-- ============================================================================
-- CLICK DISPATCHER
-- ============================================================================

local click_handlers = {}

function M.click(minwid, _, button, _)
  local fn = click_handlers[minwid]
  if fn then fn(button) end
end

local function clickable(id, fn, content)
  click_handlers[id] = fn
  return string.format("%%%d@v:lua.Lib.statusline.click@%s%%X", id, content)
end

-- ============================================================================
-- CACHE
-- ============================================================================
--
-- Each buffer gets its own cache in vim.b[buf]._sl_cache (a table of strings).
-- Segments re-compute only when their invalidation event fires.
-- render() is the tight hot path: read cache, concat, done.

local function cache_of(buf)
  local c = vim.b[buf]._sl_cache
  if not c then
    c = {}
    vim.b[buf]._sl_cache = c
  end
  return c
end

local function invalidate(buf, ...)
  local c = vim.b[buf]._sl_cache
  if not c then return end
  for _, key in ipairs({ ... }) do c[key] = nil end
  vim.b[buf]._sl_cache = c
end

-- ============================================================================
-- SEGMENT BUILDERS (cache-backed)
-- ============================================================================

local function build_macro()
  local reg = vim.fn.reg_recording()
  if reg == "" then return "" end
  return "%#StslMacro# ● REC @" .. reg .. " "
end

local function build_git(buf)
  local g = vim.b[buf].gitsigns_status_dict
  if not g or not g.head or g.head == "" then return "" end
  local G = Lib.icons.git
  local parts = { "%#StslGit# " .. g.head }
  if (g.added   or 0) > 0 then parts[#parts + 1] = "%#StslGitAdd#"    .. G.added    .. g.added   end
  if (g.changed or 0) > 0 then parts[#parts + 1] = "%#StslGitChange#" .. G.modified .. g.changed end
  if (g.removed or 0) > 0 then parts[#parts + 1] = "%#StslGitDel#"    .. G.removed  .. g.removed end
  local content = table.concat(parts, " ") .. " "
  return clickable(1, function() pcall(function() Snacks.picker.git_log() end) end, content)
end

-- Get the path-like name for a buffer. For oil:// buffers we resolve to
-- the actual directory being edited so the statusline shows sensible
-- path segments instead of the literal URI.
local function buf_path_name(buf)
  local ft = vim.bo[buf].filetype
  if ft == "oil" then
    local ok, oil = pcall(require, "oil")
    if ok and oil.get_current_dir then
      return oil.get_current_dir(buf) or ""
    end
  end
  return vim.api.nvim_buf_get_name(buf)
end

-- Split an absolute path into (cwd_display, rel_dir, basename) for the
-- tri-color renderer. home → `~` in the cwd zone. Directory paths keep
-- their trailing slash on the basename so the user can see it's a dir.
-- Edge case: if abs IS cwd (or a trailing-slashed variant — oil on cwd),
-- we break cwd into parent + name so the three zones don't duplicate.
local function split_path_zones(abs)
  local cwd  = vim.uv.cwd() or ""
  local home = vim.env.HOME or ""
  local function tilde(p)
    if home ~= "" and p:sub(1, #home) == home then return "~" .. p:sub(#home + 1) end
    return p
  end

  local is_dir = abs:sub(-1) == "/"
  local stripped = is_dir and abs:sub(1, -2) or abs
  local trail = is_dir and "/" or ""

  if stripped == cwd then
    -- The buffer IS cwd (directory browser on cwd). Show the parent in
    -- the dim cwd zone and the cwd's own name as the basename.
    local parent = vim.fn.fnamemodify(cwd, ":h")
    local cwd_name = vim.fn.fnamemodify(cwd, ":t")
    local cwd_zone = parent ~= "" and parent ~= "." and (tilde(parent) .. "/") or ""
    return cwd_zone, "", cwd_name .. trail
  end

  local basename = vim.fn.fnamemodify(stripped, ":t") .. trail

  if stripped:sub(1, #cwd + 1) == cwd .. "/" then
    -- Inside cwd: dim cwd zone + accent relative zone + bright basename.
    local rel  = stripped:sub(#cwd + 2)
    local head = vim.fn.fnamemodify(rel, ":h")
    local reldir = (head == "." or head == "") and "" or (head .. "/")
    return tilde(cwd) .. "/", reldir, basename
  end

  -- Outside cwd: no cwd zone, just a home-relative parent dir.
  local head = vim.fn.fnamemodify(stripped, ":h")
  return "", tilde(head) .. "/", basename
end

-- Display width of a statusline-encoded string (strips %# / %@ / etc.).
local function strwidth(s)
  local ok, r = pcall(vim.api.nvim_eval_statusline, s, {})
  return ok and r.width or vim.fn.strdisplaywidth(s)
end

-- Progressive path render: given a budget (display columns), pick the
-- widest rendering that fits. Levels, longest-to-shortest:
--   0: cwd_zone + rel_zone + basename
--   1: cwd_zone + pathshorten(rel_zone) + basename
--   2: pathshorten(rel_zone) + basename (cwd dropped)
--   3: basename only
--   4: truncated basename with ellipsis
local function build_path(buf, budget)
  local name = buf_path_name(buf)
  if name == "" then return "%#StslDim# [No Name] " end

  local abs = vim.fn.fnamemodify(name, ":p")
  local cwd_zone, rel_zone, base = split_path_zones(abs)

  local base_hl = "StslPathFile"
  if vim.bo[buf].modified then base_hl = "StslPathMod"
  elseif vim.bo[buf].readonly or not vim.bo[buf].modifiable then base_hl = "StslPathRO" end

  local mark = ""
  if vim.bo[buf].modified then
    mark = mark .. "%#StslMod# " .. Lib.icons.git.modified
  end
  if vim.bo[buf].readonly or not vim.bo[buf].modifiable then
    mark = mark .. "%#StslRO# " .. Lib.icons.status.Lock
  end

  local function compose(cwd, rel, b)
    local p = { " " }
    if cwd and cwd ~= "" then p[#p + 1] = "%#StslPathCwd#" .. cwd end
    if rel and rel ~= "" then p[#p + 1] = "%#StslPathRel#" .. rel end
    p[#p + 1] = "%#" .. base_hl .. "#" .. b
    if mark ~= "" then p[#p + 1] = mark end
    p[#p + 1] = " "
    return table.concat(p)
  end

  local short_rel = rel_zone ~= "" and vim.fn.pathshorten(rel_zone) or ""
  local levels = {
    compose(cwd_zone, rel_zone,  base),   -- 0: full
    compose(cwd_zone, short_rel, base),   -- 1: shorten rel
    compose("",       short_rel, base),   -- 2: drop cwd
    compose("",       "",        base),   -- 3: basename only
  }
  if not budget or budget <= 0 then return levels[1] end
  for _, s in ipairs(levels) do
    if strwidth(s) <= budget then return s end
  end
  -- Fallback: truncate basename from the middle
  local shell = compose("", "", "")
  local remaining = budget - strwidth(shell)
  if remaining >= 3 then
    local head = math.floor((remaining - 1) / 2)
    local tail = remaining - 1 - head
    local truncated = base:sub(1, head) .. "…" .. base:sub(-tail)
    return compose("", "", truncated)
  end
  return levels[#levels]
end

local function build_diag(buf)
  local counts = vim.diagnostic.count(buf)
  if next(counts) == nil then return "" end
  local E = vim.diagnostic.severity
  local I = Lib.icons.diagnostics
  local parts = {}
  if (counts[E.ERROR] or 0) > 0 then parts[#parts + 1] = "%#StslDiagErr# "  .. I.Error .. counts[E.ERROR] end
  if (counts[E.WARN]  or 0) > 0 then parts[#parts + 1] = "%#StslDiagWarn# " .. I.Warn  .. counts[E.WARN]  end
  if (counts[E.INFO]  or 0) > 0 then parts[#parts + 1] = "%#StslDiagInfo# " .. I.Info  .. counts[E.INFO]  end
  if (counts[E.HINT]  or 0) > 0 then parts[#parts + 1] = "%#StslDiagHint# " .. I.Hint  .. counts[E.HINT]  end
  if #parts == 0 then return "" end
  local content = table.concat(parts, " ") .. " "
  return clickable(2, function() pcall(function() Snacks.picker.diagnostics_buffer() end) end, content)
end

local function build_lsp(buf)
  local clients = vim.lsp.get_clients({ bufnr = buf })
  if #clients == 0 then return "" end
  local width = vim.api.nvim_win_get_width(0)
  local names
  if width < 80 then
    names = "lsp×" .. #clients
  else
    local n = {}
    for _, c in ipairs(clients) do n[#n + 1] = c.name end
    names = table.concat(n, ",")
  end
  return clickable(3, function() vim.cmd("checkhealth vim.lsp") end, "%#StslLsp# " .. Lib.icons.status.Lsp .. names .. " ")
end

local function pick_filetype()
  local items = vim.fn.getcompletion("", "filetype")
  table.sort(items)
  vim.ui.select(items, { prompt = "Set filetype: " }, function(choice)
    if choice and choice ~= "" then vim.bo.filetype = choice end
  end)
end

local function build_filetype(buf)
  local ft = vim.bo[buf].filetype
  if ft == "" then ft = "plain" end
  local icon = ""
  if ft == "oil" or ft == "netrw" or ft == "neo-tree" then
    icon = Lib.icons.status.Directory
  else
    local ok, devicons = pcall(require, "nvim-web-devicons")
    if ok then
      local name = vim.api.nvim_buf_get_name(buf)
      local ext  = vim.fn.fnamemodify(name, ":e")
      icon = (devicons.get_icon(name, ext, { default = true }) or "") .. " "
    end
  end
  return clickable(6, pick_filetype, "%#StslFT# " .. icon .. ft:upper() .. " ")
end

local function build_enc(buf)
  local enc = vim.bo[buf].fileencoding
  if enc == "" or enc == "utf-8" then return "" end
  return "%#StslEnc# " .. enc .. " "
end

local function build_bom(buf)
  if vim.bo[buf].bomb then return "%#StslBom# BOM " end
  return ""
end

local function build_fmt(buf)
  local fmt = vim.bo[buf].fileformat
  if fmt == "unix" then return "" end
  return "%#StslFmt# " .. fmt .. " "
end

-- Auto-format state indicator. Hidden when the default is active (global on,
-- no buffer override). Shown only when state is "unusual" — global off, or
-- a per-buffer override is set. Clickable: toggles the current scope.
local function build_autofmt(buf)
  local gaf = vim.g.autoformat == nil or vim.g.autoformat
  local baf = vim.b[buf].autoformat
  if gaf and baf == nil then return "" end

  local effective = baf == nil and gaf or baf
  local label, hl
  if baf == nil then        -- global override (turned off for everyone)
    label, hl = "AF-OFF", "StslAFOff"
  elseif baf then           -- buffer says ON (usually when global is OFF)
    label, hl = "AF-ON(buf)", "StslAFOn"
  else                      -- buffer says OFF
    label, hl = "AF-OFF(buf)", "StslAFOff"
  end
  local _ = effective  -- silence unused; label already encodes state
  -- Click: toggle whichever scope this buffer is actually responding to.
  return clickable(20, function() Lib.format.toggle(baf ~= nil) end,
    "%#" .. hl .. "#  " .. label .. " ")
end

-- NOT cached — state changes per keystroke but this is fast
local function build_spell()
  if not vim.wo.spell then return "" end
  return "%#StslSpell# 󰓆 " .. table.concat(vim.opt_local.spelllang:get(), ",") .. " "
end

local function build_snippet()
  if vim.snippet and vim.snippet.active() then
    return "%#StslSnip# ● snippet "
  end
  return ""
end

local function build_search()
  if package.loaded["noice"] or vim.v.hlsearch == 0 then return "" end
  local ok, res = pcall(vim.fn.searchcount, { maxcount = 999, timeout = 10 })
  if not ok or not res or (res.total or 0) == 0 then return "" end
  if res.incomplete == 1 then return "%#StslSearch#  ?/? " end
  return string.format("%%#StslSearch#  %d/%d ", res.current, res.total)
end

local function build_dap()
  if not package.loaded["dap"] then return "" end
  local dap = require("dap")
  local status = dap.status and dap.status() or ""
  if status == "" then return "" end
  local buttons = {
    clickable(10, function() dap.step_into() end, " "),
    clickable(11, function() dap.step_over() end, " "),
    clickable(12, function() dap.step_out() end,  " "),
    clickable(13, function() dap.continue() end,  " "),
    clickable(14, function() dap.terminate() end, " "),
  }
  return "%#StslDap# " .. Lib.icons.status.Debug .. status .. " " .. table.concat(buttons, "") .. " "
end

local function build_pos()
  return "%#StslPos# %l:%c "
end

local function build_scroll()
  local lnum = vim.fn.line(".")
  local total = vim.fn.line("$")
  return "%#StslScroll#" .. M._segments.scrollbar(lnum, total)
end

local function end_cap()
  return "%#StslCapR#" .. SEP.cap_r
end

-- ============================================================================
-- SPECIAL BUFFER VARIANTS
-- ============================================================================

local function render_special(buf)
  local bt = vim.bo[buf].buftype
  if bt == "quickfix" then
    local loc = vim.fn.getloclist(0, { filewinid = 1 }).filewinid ~= 0
    local label = loc and "Location List" or "Quickfix List"
    local title = vim.w.quickfix_title or ""
    return M._segments.mode()
      .. "%#StslBold# " .. label .. "%*%#Stsl# " .. title .. " "
      .. "%=" .. build_pos() .. build_scroll() .. end_cap()
  elseif bt == "terminal" then
    local name = vim.api.nvim_buf_get_name(buf):gsub("^term://", "")
    return M._segments.mode() .. "%#StslBold#  " .. name .. "%=" .. build_pos() .. end_cap()
  elseif bt == "help" then
    local name = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t")
    return M._segments.mode() .. "%#StslBold#  " .. name
      .. "%=%#StslFT# HELP " .. build_pos() .. end_cap()
  end
  return nil
end

-- ============================================================================
-- RENDER (hot path)
-- ============================================================================

-- Inactive render: dim file path only, no segments. Quick + cheap.
local function render_inactive(buf)
  local name = vim.api.nvim_buf_get_name(buf)
  if name == "" then name = "[No Name]" else name = vim.fn.fnamemodify(name, ":~:.") end
  local mod = vim.bo[buf].modified and " [+]" or ""
  return "%#StslNc#  " .. name .. mod .. " %=%#StslNc# %l:%c "
end

function M.render()
  -- vim.g.statusline_winid tells us which window we're rendering for
  -- (relevant for laststatus=2; for laststatus=3 it's the current win).
  local winid = vim.g.statusline_winid or vim.api.nvim_get_current_win()
  local buf = vim.api.nvim_win_get_buf(winid)
  local is_current = winid == vim.api.nvim_get_current_win()

  if not is_current then return render_inactive(buf) end

  -- Special variants short-circuit
  local bt = vim.bo[buf].buftype
  if bt == "quickfix" or bt == "terminal" or bt == "help" then
    local s = render_special(buf)
    if s then return s end
  end

  local c = cache_of(buf)

  if c.git       == nil then c.git       = build_git(buf) end
  if c.diag      == nil then c.diag      = build_diag(buf) end
  if c.lsp       == nil then c.lsp       = build_lsp(buf) end
  if c.ft        == nil then c.ft        = build_filetype(buf) end
  if c.enc       == nil then c.enc       = build_enc(buf) end
  if c.bom       == nil then c.bom       = build_bom(buf) end
  if c.fmt       == nil then c.fmt       = build_fmt(buf) end

  vim.b[buf]._sl_cache = c  -- write-back

  -- Right-side segments: listed in natural render order, each annotated
  -- with a drop priority (lower = dropped first). Essentials use priority
  -- 99 so they're never dropped.
  local right_segs = {
    { key = "diag",    content = c.diag,           priority = 65 },
    { key = "dap",     content = build_dap(),      priority = 60 },
    { key = "lsp",     content = c.lsp,            priority = 55 },
    { key = "autofmt", content = build_autofmt(buf), priority = 70 },
    { key = "ft",      content = c.ft,             priority = 45 },
    { key = "enc",     content = c.enc,            priority = 10 },
    { key = "bom",     content = c.bom,            priority = 5  },
    { key = "fmt",     content = c.fmt,            priority = 5  },
    { key = "spell",   content = build_spell(),    priority = 20 },
    { key = "snippet", content = build_snippet(),  priority = 25 },
    { key = "search",  content = build_search(),   priority = 25 },
    { key = "scroll",  content = build_scroll(),   priority = 30 },
    { key = "pos",     content = build_pos(),      priority = 95 },
    { key = "cap",     content = end_cap(),        priority = 99 },
  }

  local left   = M._segments.mode() .. build_macro() .. c.git
  local win_w  = vim.api.nvim_win_get_width(winid)
  local left_w = strwidth(left)

  -- Reserve enough budget for a truncated basename (≥ 6 cells) plus
  -- left+right decoration. Drop lowest-priority right segments until we
  -- have headroom. Essentials (priority ≥ 95) are never dropped.
  local PATH_MIN = 6
  for _, s in ipairs(right_segs) do
    s.width  = strwidth(s.content)
    s.keep   = true
  end

  local function right_total()
    local t = 0
    for _, s in ipairs(right_segs) do if s.keep then t = t + s.width end end
    return t
  end

  while left_w + right_total() + PATH_MIN + 1 > win_w do
    local victim_idx, victim_pri = nil, math.huge
    for i, s in ipairs(right_segs) do
      if s.keep and s.priority < 95 and s.priority < victim_pri and s.width > 0 then
        victim_idx, victim_pri = i, s.priority
      end
    end
    if not victim_idx then break end  -- nothing more to drop
    right_segs[victim_idx].keep = false
  end

  local right_parts = {}
  for _, s in ipairs(right_segs) do
    if s.keep then right_parts[#right_parts + 1] = s.content end
  end
  local right = table.concat(right_parts)

  local path_budget = win_w - left_w - strwidth(right) - 1
  local path = build_path(buf, path_budget)

  return left .. path .. "%=" .. right
end

-- ============================================================================
-- SETUP
-- ============================================================================

function M.setup()
  define_highlights()
  require("lib.mode_color").setup()

  -- Re-define highlights on colorscheme change
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("Lib.statusline.hl", { clear = true }),
    callback = function()
      -- Catppuccin may not be loaded yet during early colorscheme events; protect.
      pcall(define_highlights)
    end,
  })

  -- Per-segment cache invalidation
  local grp = vim.api.nvim_create_augroup("Lib.statusline.cache", { clear = true })
  local function on(events, opts, keys)
    vim.api.nvim_create_autocmd(events, vim.tbl_extend("force", { group = grp,
      callback = function(args)
        local buf = args.buf or vim.api.nvim_get_current_buf()
        invalidate(buf, unpack(keys))
        pcall(vim.cmd, "redrawstatus")
      end,
    }, opts or {}))
  end

  on({ "BufEnter", "BufWinEnter", "BufWritePost", "FileChangedShellPost" }, {},
     { "git", "path", "diag", "lsp", "ft", "enc", "bom", "fmt" })
  on({ "DiagnosticChanged" }, {}, { "diag" })
  on({ "LspAttach", "LspDetach" }, {}, { "lsp" })
  on({ "User" }, { pattern = "GitSignsUpdate" }, { "git" })
  on({ "OptionSet" }, { pattern = "fileformat,fileencoding,bomb,filetype" }, { "enc", "bom", "fmt", "ft" })
  on({ "BufModifiedSet" }, {}, { "path" })

  -- Force redraw for transient states (mode glyphs, recording, search).
  -- WinResized / VimResized trigger the path-budget recomputation so the
  -- filepath zone narrows/widens smoothly as the user drags pane splits.
  vim.api.nvim_create_autocmd({
    "RecordingEnter", "RecordingLeave", "CmdlineLeave", "SearchWrapped",
    "WinEnter", "WinLeave", "WinResized", "VimResized",
  }, {
    group = grp,
    callback = function() pcall(vim.cmd, "redrawstatus!") end,
  })
end

-- Legacy entry (kept for test compatibility)
function M.handle_click(_, _, button, _)
  if button == "l" then pcall(function() Snacks.picker.diagnostics_buffer() end) end
end

return M
