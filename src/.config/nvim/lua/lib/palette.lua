-- Derive palette from CURRENT colorscheme's standard highlight groups.
-- Works with any theme (catppuccin, tokyonight, gruvbox, kanagawa, etc.)
-- because we reference semantic groups every colorscheme defines.

local color = require("lib.colors.color")

local M = {}

function M.hl_hex(name, attr)
  local v = vim.api.nvim_get_hl(0, { name = name, link = false })[attr]
  if type(v) == "number" then return string.format("#%06x", v) end
  return nil
end

local function get(name, attr, fallback)
  return M.hl_hex(name, attr) or fallback
end

function M.shade(hex, delta)
  if not hex or hex == "NONE" then return hex end
  local c = color.from_hex(hex)
  local d = delta / 255
  c.r, c.g, c.b = c.r + d, c.g + d, c.b + d
  return color.to_hex(c)
end

function M.mix(from, to, t)
  local a, b = color.from_hex(from), color.from_hex(to)
  return color.to_hex({
    r = a.r + (b.r - a.r) * t,
    g = a.g + (b.g - a.g) * t,
    b = a.b + (b.b - a.b) * t,
  })
end

local current

function M.refresh()
  local bg_end   = get("Normal", "bg", "#000000")
  -- Recessed (darker than Normal.bg) bar -- modern minimal look. Catppuccin
  -- mocha sets StatusLine.bg to mantle (#181825), which is darker than
  -- Normal.bg (base #1e1e2e) and gives crisp contrast for all fg colors.
  local bg_mid   = get("StatusLine", "bg", nil) or get("Folded", "bg", nil) or bg_end
  -- Main fg comes from Normal, not StatusLine (StatusLine.fg is often dimmed)
  local fg       = get("Normal", "fg", "#ffffff")
  -- fg_dim must clear WCAG AA (4.5:1) on bg_mid. LineNr (#45475a / surface1)
  -- and NonText (#6c7086 / overlay0) both fail on mantle (~1.9:1 and ~4.0:1).
  -- Conceal.fg is overlay1 (#7f849c) which gives ~5.3:1 on mantle -- PASS.
  local fg_dim   = get("Conceal", "fg", nil) or get("Comment", "fg", nil) or get("NonText", "fg", fg)
  -- Path cwd specifically: bumped further for clear legibility (was the
  -- "current dir hard to see" complaint). Pmenu.fg is overlay2 in mocha
  -- (#9399b2); Conceal.fg is overlay1 (#7f849c) -- use Pmenu first.
  local fg_path_cwd = get("Pmenu", "fg", nil) or get("Conceal", "fg", nil) or fg_dim

  -- Semantic source groups -- every theme defines these
  local red      = get("DiagnosticError", "fg", nil) or get("Error",       "fg", "#ff5555")
  local yellow   = get("DiagnosticWarn",  "fg", nil) or get("WarningMsg",  "fg", "#f1fa8c")
  local green    = get("String",          "fg", nil) or "#50fa7b"

  current = {
    bg_end = bg_end, bg_mid = bg_mid, fg = fg, fg_bold = fg, fg_dim = fg_dim,
    fg_path_cwd = fg_path_cwd,
    red = red, yellow = yellow, green = green,
    blue    = get("Function",       "fg", nil) or "#8be9fd",
    purple  = get("Statement",      "fg", nil) or get("Keyword",     "fg", "#bd93f9"),
    orange  = get("Constant",       "fg", nil) or get("Number",      "fg", "#ffb86c"),
    cyan    = get("Special",        "fg", nil) or get("Type",        "fg", "#8be9fd"),
    teal    = get("Type",           "fg", nil) or get("Special",     "fg", "#8be9fd"),
    peach   = get("Number",         "fg", nil) or get("Constant",    "fg", "#ffb86c"),
    mauve   = get("Keyword",        "fg", nil) or get("Statement",   "fg", "#ff79c6"),
    info    = get("DiagnosticInfo", "fg", nil) or get("Function",    "fg", "#8be9fd"),
    hint    = get("DiagnosticHint", "fg", nil) or get("Special",     "fg", "#50fa7b"),
    git_add = get("diffAdded",      "fg", nil) or green,
    git_del = get("diffRemoved",    "fg", nil) or get("diffDeleted", "fg", red),
    git_chg = get("diffChanged",    "fg", nil) or yellow,
  }
  return current
end

function M.get()
  return current or M.refresh()
end

return M
