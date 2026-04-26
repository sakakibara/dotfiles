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
    return
  end
  local top, bot = viewport(buf)
  local detected = D().detect(buf, top, bot)
  R().apply(buf, detected)
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
    pcall(TW().scan_project)
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
end

function M.toggle(buf)
  if not buf or buf == 0 then buf = vim.api.nvim_get_current_buf() end
  -- Default state is "enabled" (nil); first toggle should flip to false.
  M._enabled[buf] = not (M._enabled[buf] ~= false)
  redraw(buf)
end

return M
