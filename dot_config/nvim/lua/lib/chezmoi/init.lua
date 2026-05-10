-- nvim integration for chezmoi templates (*.tmpl files):
--   1. Filetype detection — strip .tmpl, set ft to underlying language so
--      LSP / formatters / treesitter for that language attach normally.
--   2. Treesitter injection — attach gotmpl as a child language tree scoped
--      to {{ ... }} regions of the buffer. Inside directives you get full
--      gotmpl semantic highlighting; outside, host language highlighting is
--      unaffected.
--   3. Custom blink.cmp source — see lib/chezmoi/blink.lua. Registered in
--      the blink.cmp spec under sources.providers.chezmoi.

local M = {}

-- ---------- helpers ----------

-- Map a chezmoi-source filename (e.g. "dot_zshrc") to its underlying ft.
-- Falls back to vim.filetype.match for everything else.
local CHEZMOI_FT_FALLBACKS = {
  zshrc        = "zsh",
  zshenv       = "zsh",
  zprofile     = "zsh",
  zlogin       = "zsh",
  bashrc       = "bash",
  bash_profile = "bash",
  vimrc        = "vim",
  ["tmux.conf"]= "tmux",
  gitconfig    = "gitconfig",
  ["ssh/config"] = "sshconfig",
}

local function detect_ft(filename)
  local stripped = filename:gsub("%.tmpl$", "")
  local ft = vim.filetype.match({ filename = stripped })
  if ft then return ft end
  for suffix, mapped in pairs(CHEZMOI_FT_FALLBACKS) do
    if stripped:match(suffix .. "$") then return mapped end
  end
  return nil
end

-- Scan the buffer for {{ ... }} regions. Returns a list of treesitter
-- regions, where each region is a list of Range6 tuples
-- {start_row, start_col, start_byte, end_row, end_col, end_byte}.
--
-- Single-line directives only — multi-line {{ ... }} is rare in chezmoi
-- templates and would need a different scanner (full-buffer scan with
-- byte/row/col bookkeeping). Add when actually needed.
function M._template_regions(buf)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local regions = {}
  local line_start_byte = 0
  for lnum, line in ipairs(lines) do
    local row = lnum - 1
    local pos = 1
    while pos <= #line do
      local s, e = line:find("%{%{.-%}%}", pos)
      if not s then break end
      local scol, ecol = s - 1, e
      table.insert(regions, {
        { row, scol, line_start_byte + scol, row, ecol, line_start_byte + ecol },
      })
      pos = e + 1
    end
    line_start_byte = line_start_byte + #line + 1  -- + newline
  end
  return regions
end

-- Attach gotmpl as a child language tree of the host parser, scoped to
-- the buffer's current {{ ... }} regions. Best-effort: if treesitter or
-- the gotmpl parser isn't available, silently skip — nvim's normal
-- highlighting still works.
local function refresh_injection(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  if not vim.b[buf].chezmoi_template then return end

  local ok, host = pcall(vim.treesitter.get_parser, buf)
  if not ok or not host then return end

  local regions = M._template_regions(buf)

  -- Add or get the gotmpl child. add_child errors if the parser isn't
  -- installed; pcall lets us degrade gracefully.
  local children = host:children()
  local child = children["gotmpl"]
  if not child then
    local added = pcall(host.add_child, host, "gotmpl")
    if not added then return end
    child = host:children()["gotmpl"]
  end

  if child and #regions > 0 then
    pcall(child.set_included_regions, child, regions)
    pcall(child.parse, child)
  elseif child then
    pcall(child.set_included_regions, child, {})
  end
end

-- Debounce: TextChangedI fires per-keystroke; deferring to vim.schedule
-- coalesces bursts and avoids re-parsing on every character.
local _scheduled = {}
local function schedule_refresh(buf)
  if _scheduled[buf] then return end
  _scheduled[buf] = true
  vim.schedule(function()
    _scheduled[buf] = nil
    refresh_injection(buf)
  end)
end

-- ---------- public API ----------

function M.setup()
  local au = vim.api.nvim_create_autocmd
  local grp = vim.api.nvim_create_augroup("Lib.chezmoi", { clear = true })

  -- Filetype detection: strip .tmpl, route to underlying language. Mark
  -- the buffer with b:chezmoi_template so blink.cmp's source and the
  -- highlighter both know to act.
  au({ "BufNewFile", "BufReadPost" }, {
    group = grp,
    pattern = "*.tmpl",
    callback = function(ev)
      local ft = detect_ft(ev.file)
      if not ft then return end
      vim.bo[ev.buf].filetype = ft
      vim.b[ev.buf].chezmoi_template = true
      schedule_refresh(ev.buf)
    end,
  })

  -- Re-scan ranges on edits. Coalesced via vim.schedule (single
  -- refresh per turn even for bursts of TextChangedI).
  au({ "TextChanged", "TextChangedI", "BufWinEnter" }, {
    group = grp,
    callback = function(ev)
      if vim.b[ev.buf].chezmoi_template then schedule_refresh(ev.buf) end
    end,
  })
end

return M
