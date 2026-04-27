-- lua/lib/colors/init.lua
-- Public entry point. setup() registers autocmds + commands; toggle()
-- enables/disables per-buffer or globally.

local M = {}

-- Lazy accessors so tests can clear package.loaded and get fresh modules.
local function D()  return require("lib.colors.detect")   end
local function R()  return require("lib.colors.render")   end
local function TW() return require("lib.colors.tailwind") end

local DEFAULTS = {
  enabled         = true,
  exclude_ft      = { "bigfile" },
  exclude_bt      = { "terminal", "prompt", "quickfix" },
  tailwind        = { project_scan = true },
  debounce_ms     = 16,
  regex_filetypes = {},   -- opt-in for regex fallback (TS-less filetypes get no detection)
}

M._opts    = DEFAULTS
M._timers  = {}   -- per-buffer debouncer
M._enabled = {}   -- per-buffer toggle override

local function is_excluded(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return true end
  if not M._opts.enabled then return true end
  local bt = vim.bo[buf].buftype
  for _, x in ipairs(M._opts.exclude_bt) do
    if bt == x then return true end
  end
  local ft = vim.bo[buf].filetype
  for _, x in ipairs(M._opts.exclude_ft) do
    if ft == x then return true end
  end
  if M._enabled[buf] == false then return true end
  return false
end

local function viewport(buf)
  -- For headless tests there's no real window; fall back to whole buffer.
  local win = vim.fn.bufwinid(buf)
  if win == -1 then
    return 0, math.max(0, vim.api.nvim_buf_line_count(buf) - 1)
  end
  local top = vim.api.nvim_win_call(win, function() return vim.fn.line("w0") end) - 1
  local bot = vim.api.nvim_win_call(win, function() return vim.fn.line("w$") end) - 1
  return top, bot
end

local function redraw(buf)
  if is_excluded(buf) then
    R().clear(buf)
    require("lib.colors.contrast").clear(buf)
    return
  end
  local top, bot = viewport(buf)
  local detected = D().detect(buf, top, bot)
  R().apply(buf, detected)
  local CT = require("lib.colors.contrast")
  if CT.is_enabled(buf) then CT.render(buf, top, bot, detected) end
end

local function schedule(buf)
  local t = M._timers[buf]
  if not t then
    t = vim.uv.new_timer()
    M._timers[buf] = t
  else
    t:stop()
  end
  t:start(M._opts.debounce_ms, 0, vim.schedule_wrap(function()
    if vim.api.nvim_buf_is_valid(buf) then redraw(buf) end
  end))
end

function M.setup(opts)
  M._opts = vim.tbl_deep_extend("force", DEFAULTS, opts or {})

  -- Reset and populate the regex-fallback ft set
  D()._regex_filetypes = {}
  for _, ft in ipairs(M._opts.regex_filetypes or {}) do
    D()._regex_filetypes[ft] = true
  end

  if M._opts.tailwind and M._opts.tailwind.project_scan then
    -- Defer scan_project: even with directory-level pruning, monorepos with
    -- hundreds of CSS files take measurable wall time. Scheduling lets nvim
    -- finish startup, then the @theme overlay populates within a tick or two.
    vim.schedule(function() pcall(TW().scan_project) end)
  end

  local grp = vim.api.nvim_create_augroup("Lib.colors", { clear = true })
  vim.api.nvim_create_autocmd({
    "BufReadPost", "BufWinEnter", "TextChanged", "TextChangedI",
    "WinScrolled", "WinResized",
  }, {
    group = grp,
    callback = function(args) schedule(args.buf) end,
  })

  vim.api.nvim_create_autocmd("DirChanged", {
    group   = grp,
    pattern = { "global", "tabpage" },
    callback = function()
      if M._opts.tailwind and M._opts.tailwind.project_scan then
        local TW_mod = require("lib.colors.tailwind")
        TW_mod._overlay         = {}
        TW_mod._overlay_by_file = {}
        pcall(TW_mod.scan_project)
      end
    end,
  })

  -- Re-scan project CSS files when one is saved.
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = grp,
    pattern = "*.css",
    callback = function(args)
      if M._opts.tailwind and M._opts.tailwind.project_scan then
        pcall(TW().scan_file, vim.api.nvim_buf_get_name(args.buf))
      end
    end,
  })

  vim.api.nvim_create_autocmd("BufDelete", {
    group = grp,
    callback = function(args)
      if M._timers[args.buf] then
        M._timers[args.buf]:stop()
        M._timers[args.buf]:close()
        M._timers[args.buf] = nil
      end
      R().clear(args.buf)
    end,
  })

  vim.api.nvim_create_user_command("ColorPick", function() M.pick() end, {
    desc = "Open Lib.colors picker on color under cursor",
  })

  vim.api.nvim_create_user_command("ColorsToggle", function(cmd)
    if cmd.fargs[1] == "global" then
      M._opts.enabled = not M._opts.enabled
      for buf in pairs(M._timers) do redraw(buf) end
    else
      M.toggle(0)
    end
  end, {
    desc     = "Toggle Lib.colors highlights for current buffer (or global)",
    nargs    = "?",
    complete = function() return { "global" } end,
  })

  local fmt_complete = function() return require("lib.colors.format").formats() end
  vim.api.nvim_create_user_command("ColorConvert", function(cmd)
    M.convert(cmd.fargs[1])
  end, {
    desc     = "Rewrite the color literal under the cursor in <fmt>",
    nargs    = 1,
    complete = fmt_complete,
  })
  vim.api.nvim_create_user_command("ColorYank", function(cmd)
    M.yank(cmd.fargs[1])
  end, {
    desc     = "Yank the color under the cursor in <fmt> to the + register",
    nargs    = 1,
    complete = fmt_complete,
  })
  vim.api.nvim_create_user_command("ColorContrast", function()
    local on = require("lib.colors.contrast").toggle(0)
    vim.notify("Contrast hints " .. (on and "on" or "off"), vim.log.levels.INFO)
  end, {
    desc = "Toggle WCAG contrast ratio virt-text for the current buffer",
  })
end

-- Find the color literal under the cursor and call `cb(text, hit)` on
-- it. Reports a notify when nothing's under the cursor or `fmt` is bogus,
-- and centralizes the "where am I, what's here" plumbing.
local function with_color_under_cursor(fmt, cb)
  local format = require("lib.colors.format")
  if not format.is_format(fmt) then
    vim.notify("Lib.colors: unknown format '" .. tostring(fmt) ..
               "' (expected one of: " .. table.concat(format.formats(), ", ") .. ")",
               vim.log.levels.ERROR)
    return
  end
  local parse = require("lib.colors.parse")
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  local line = vim.api.nvim_buf_get_lines(0, row - 1, row, false)[1] or ""
  local hit  = parse.parse(line, col)
  if not hit then
    vim.notify("Lib.colors: no color literal under the cursor", vim.log.levels.WARN)
    return
  end
  local text = format.format(hit.color, fmt, hit.color.source)
  cb(text, hit, row)
end

function M.convert(fmt)
  with_color_under_cursor(fmt, function(text, hit, row)
    local buf = vim.api.nvim_get_current_buf()
    vim.api.nvim_buf_set_text(buf, row - 1, hit.range.col_s, row - 1, hit.range.col_e, { text })
  end)
end

function M.yank(fmt)
  with_color_under_cursor(fmt, function(text)
    vim.fn.setreg("+", text)
    vim.fn.setreg('"', text)
    vim.notify("Yanked " .. text, vim.log.levels.INFO)
  end)
end

function M.pick(initial)
  local picker = require("lib.colors.picker")
  local parse  = require("lib.colors.parse")
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  local line = vim.api.nvim_buf_get_lines(0, row - 1, row, false)[1] or ""
  local hit = parse.parse(line, col)
  local buf = vim.api.nvim_get_current_buf()
  local opts = { initial = initial }
  if hit then
    -- Replace existing literal in place
    opts.initial = hit.color
    opts.anchor  = { buf = buf, lnum = row - 1,
                     col_s = hit.range.col_s, col_e = hit.range.col_e }
  else
    -- Insert at cursor (zero-width range; nvim_buf_set_text with start==end inserts)
    opts.anchor  = { buf = buf, lnum = row - 1, col_s = col, col_e = col }
  end
  return picker.open(opts)
end

function M.toggle(buf)
  if not buf or buf == 0 then buf = vim.api.nvim_get_current_buf() end
  -- Default state is "enabled" (nil); first toggle should flip to false.
  M._enabled[buf] = not (M._enabled[buf] ~= false)
  redraw(buf)
end

return M
