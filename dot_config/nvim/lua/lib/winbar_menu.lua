-- lua/lib/winbar_menu.lua
--
-- Floating menu used by winbar segments on click or keyboard invocation.
-- Drill-down:  l / <Right>     — dive into a directory / symbol scope
--              h / <Left>      — pop back to parent menu
-- Selection:   <CR> / <2-Click>— commit the item (cd, :edit, jump to symbol)
-- Movement:    j k <Down> <Up> g G <C-u> <C-d>
-- Dismiss:     q <Esc> — close menu, return focus to source window
--
-- Differs from a modal picker: anchored at the clicked/triggering column,
-- dismisses on focus loss via WinClosed (not WinLeave/BufLeave, which fire
-- spuriously during internal navigation).

local M = {}

-- A stack of menu frames, so drill-down can unwind to parent menus.
-- Each frame = { winid, bufnr, items, on_pick, title, anchor_col, sel }
local stack = {}
local origin_win = nil      -- window to return focus to on close
local ns = vim.api.nvim_create_namespace("Lib.winbar_menu")

local function set_hl()
  local function get(n, a)
    local h = vim.api.nvim_get_hl(0, { name = n, link = false })
    local v = h[a]
    return type(v) == "number" and string.format("#%06x", v) or nil
  end
  local normal_bg  = get("NormalFloat", "bg") or get("Pmenu",   "bg") or get("Normal", "bg")
  local normal_fg  = get("NormalFloat", "fg") or get("Normal",  "fg")
  local dim_fg     = get("Comment",     "fg") or get("NonText", "fg")
  local icon_fg    = get("Directory",   "fg") or get("Function","fg") or normal_fg
  local current_fg = get("Keyword",     "fg") or get("Statement","fg") or normal_fg
  local sel_bg     = get("PmenuSel",    "bg") or get("Visual",  "bg")
  local border_fg  = get("FloatBorder", "fg") or dim_fg
  vim.api.nvim_set_hl(0, "LibWinbarMenu",        { fg = normal_fg,  bg = normal_bg })
  vim.api.nvim_set_hl(0, "LibWinbarMenuDim",     { fg = dim_fg,     bg = normal_bg })
  vim.api.nvim_set_hl(0, "LibWinbarMenuIcon",    { fg = icon_fg,    bg = normal_bg })
  vim.api.nvim_set_hl(0, "LibWinbarMenuCurrent", { fg = current_fg, bg = normal_bg, bold = true })
  vim.api.nvim_set_hl(0, "LibWinbarMenuSel",     { bg = sel_bg, bold = true })
  vim.api.nvim_set_hl(0, "LibWinbarMenuBorder",  { fg = border_fg,  bg = normal_bg })
end

local function top()
  return stack[#stack]
end

local function close_frame(frame)
  if not frame then return end
  if frame.winid and vim.api.nvim_win_is_valid(frame.winid) then
    pcall(vim.api.nvim_win_close, frame.winid, true)
  end
  if frame.bufnr and vim.api.nvim_buf_is_valid(frame.bufnr) then
    pcall(vim.api.nvim_buf_delete, frame.bufnr, { force = true })
  end
end

function M.close_all()
  for i = #stack, 1, -1 do close_frame(stack[i]); stack[i] = nil end
  if origin_win and vim.api.nvim_win_is_valid(origin_win) then
    pcall(vim.api.nvim_set_current_win, origin_win)
  end
  origin_win = nil
end

local function pop_frame()
  local frame = table.remove(stack)
  close_frame(frame)
  local prev = top()
  if prev and prev.winid and vim.api.nvim_win_is_valid(prev.winid) then
    vim.api.nvim_set_current_win(prev.winid)
  else
    M.close_all()
  end
end

local function highlight_selected(frame, idx)
  frame.sel = idx
  vim.api.nvim_buf_clear_namespace(frame.bufnr, ns, 0, -1)
  local line_count = vim.api.nvim_buf_line_count(frame.bufnr)
  local end_row = math.min(idx, line_count)
  vim.api.nvim_buf_set_extmark(frame.bufnr, ns, idx - 1, 0, {
    end_row  = end_row,
    end_col  = 0,
    hl_group = "LibWinbarMenuSel",
    hl_eol   = true,
    priority = 100,
  })
  if frame.winid and vim.api.nvim_win_is_valid(frame.winid) then
    pcall(vim.api.nvim_win_set_cursor, frame.winid, { idx, 0 })
  end
end

local function move(delta)
  local f = top(); if not f then return end
  local cur = f.sel or 1
  local new = cur + delta
  if new < 1 then new = #f.items end
  if new > #f.items then new = 1 end
  highlight_selected(f, new)
end

local function pick_current()
  local f = top(); if not f then return end
  local idx = f.sel or 1
  local item = f.items[idx]
  local on_pick = f.on_pick
  if not item then return end
  -- If the item is a directory and `drill` is set, dive in (don't close).
  if item.is_dir and f.drill then
    local items = f.drill(item)
    if items and #items > 0 then
      M.push({ items = items, on_pick = on_pick, title = item.label, drill = f.drill })
      return
    end
  end
  M.close_all()
  if on_pick then on_pick(item) end
end

-- Render a single frame into a floating window. Called by `open` for the
-- first frame and by `push` for nested drill-down menus.
local function render_frame(frame)
  -- Build lines with a leading gutter (` ` / `●` for current) then icon + label.
  -- Highlight ranges are tracked per-line and applied as extmarks after.
  local lines = {}
  local hlspecs = {}  -- { { row, [{col_start, col_end, hl_group}, ...] }, ... }
  local widest = 0
  for i, it in ipairs(frame.items) do
    local marker   = it.current and "● " or "  "
    local icon     = (it.icon or "") ~= "" and it.icon or ""
    local icon_pad = icon ~= "" and (icon .. " ") or ""
    local label    = it.label or ""
    local line     = " " .. marker .. icon_pad .. label .. " "
    lines[i] = line

    local col = 1  -- byte column, 0-indexed: start after leading space
    local specs = {}
    specs[#specs + 1] = { col, col + #marker, it.current and "LibWinbarMenuCurrent" or "LibWinbarMenuDim" }
    col = col + #marker
    if icon_pad ~= "" then
      specs[#specs + 1] = { col, col + #icon_pad, "LibWinbarMenuIcon" }
      col = col + #icon_pad
    end
    specs[#specs + 1] = { col, col + #label, it.current and "LibWinbarMenuCurrent" or "LibWinbarMenu" }
    hlspecs[i] = specs

    local w = vim.fn.strdisplaywidth(line)
    if w > widest then widest = w end
  end

  -- Pad every line to the same width so the selected-row highlight extends
  -- uniformly and the menu looks rectangular.
  for i, line in ipairs(lines) do
    local pad = widest - vim.fn.strdisplaywidth(line)
    if pad > 0 then lines[i] = line .. string.rep(" ", pad) end
  end

  local bufnr = vim.api.nvim_create_buf(false, true)
  vim.bo[bufnr].bufhidden = "wipe"
  vim.bo[bufnr].buftype   = "nofile"
  vim.bo[bufnr].filetype  = "libwinbarmenu"
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)
  vim.bo[bufnr].modifiable = false

  -- Apply per-column highlights
  for row, specs in ipairs(hlspecs) do
    for _, s in ipairs(specs) do
      vim.api.nvim_buf_set_extmark(bufnr, ns, row - 1, s[1], {
        end_row = row - 1, end_col = s[2], hl_group = s[3], priority = 50,
      })
    end
  end

  local max_h = math.min(#lines, math.max(6, math.floor(vim.o.lines * 0.4)))
  local width = widest
  -- Clamp so the menu never overflows the right edge of the screen.
  local anchor = math.min(frame.anchor_col or 0, math.max(0, vim.o.columns - width - 2))

  local winid = vim.api.nvim_open_win(bufnr, true, {
    relative  = "editor",
    row       = 1,
    col       = anchor,
    width     = width,
    height    = max_h,
    style     = "minimal",
    border    = "rounded",
    title     = frame.title and (" " .. frame.title .. " ") or nil,
    title_pos = "left",
    zindex    = 250,
  })
  vim.wo[winid].winhl = "Normal:LibWinbarMenu,NormalFloat:LibWinbarMenu,FloatBorder:LibWinbarMenuBorder,CursorLine:LibWinbarMenuSel"
  vim.wo[winid].cursorline    = true
  vim.wo[winid].wrap          = false
  vim.wo[winid].number        = false
  vim.wo[winid].relativenumber= false
  -- Prevent horizontal scroll when a click lands past the last column.
  vim.wo[winid].sidescrolloff = 0
  -- Mouse clicks or autocmds may leave us in insert mode; force normal.
  vim.cmd.stopinsert()

  frame.winid = winid
  frame.bufnr = bufnr

  -- Jump to the item flagged `current`, else first.
  local initial = 1
  for i, it in ipairs(frame.items) do
    if it.current then initial = i; break end
  end
  highlight_selected(frame, initial)

  -- Buffer-local keymaps
  local function map(lhs, fn)
    vim.keymap.set({ "n", "i", "v" }, lhs, function()
      -- normalise mode in case the click landed us in insert
      vim.cmd.stopinsert()
      fn()
    end, { buffer = bufnr, silent = true, nowait = true })
  end
  map("j",     function() move(1)  end)
  map("k",     function() move(-1) end)
  map("<Down>",function() move(1)  end)
  map("<Up>",  function() move(-1) end)
  map("<C-d>", function() move(5)  end)
  map("<C-u>", function() move(-5) end)
  map("g",     function() highlight_selected(top(), 1) end)
  map("G",     function() highlight_selected(top(), #top().items) end)
  map("l",        pick_current)
  map("<Right>",  pick_current)
  map("<CR>",     pick_current)
  -- Single-click: just move the selection to the clicked row. Prevents
  -- Vim's default cursor-position handling from scrolling horizontally
  -- past padded lines or placing the cursor in weird spots.
  map("<LeftMouse>", function()
    local pos = vim.fn.getmousepos()
    local f = top()
    if not f or pos.winid ~= f.winid then return end
    local line = math.max(1, math.min(pos.line, #f.items))
    highlight_selected(f, line)
  end)
  -- Double-click: commit the current item.
  map("<2-LeftMouse>", pick_current)
  map("h",       function() if #stack > 1 then pop_frame() else M.close_all() end end)
  map("<Left>",  function() if #stack > 1 then pop_frame() else M.close_all() end end)
  map("q",     M.close_all)
  map("<Esc>", M.close_all)

  -- Close the whole stack when this window closes from any path except
  -- our own pop_frame (pop_frame removes the frame before close fires).
  vim.api.nvim_create_autocmd("WinClosed", {
    pattern = tostring(winid),
    callback = function()
      for _, f in ipairs(stack) do
        if f.winid == winid then M.close_all(); return end
      end
    end,
  })

  -- Close when focus leaves for a non-menu buffer (click outside, :wincmd, …).
  -- Deferred via vim.schedule so drill-down push_frame (which enters a new
  -- menu buffer) doesn't trip it — the check runs AFTER the move settles.
  vim.api.nvim_create_autocmd("BufLeave", {
    buffer = bufnr,
    callback = function()
      vim.schedule(function()
        if #stack == 0 then return end
        local cur = vim.api.nvim_get_current_buf()
        if vim.api.nvim_buf_is_valid(cur)
           and vim.bo[cur].filetype ~= "libwinbarmenu" then
          M.close_all()
        end
      end)
    end,
  })
end

-- Public: open the FIRST menu. Captures origin_win BEFORE entering the
-- float, so we can restore focus on close even though nvim_open_win(enter=true)
-- immediately transfers to the new window.
function M.open(items, opts)
  if not items or #items == 0 then return end
  M.close_all()
  set_hl()
  opts = opts or {}

  origin_win = opts.original_win or vim.api.nvim_get_current_win()

  -- Anchor under the cursor (screen column, editor-relative), clamped
  -- to viewport in render_frame. Click handlers can override with
  -- opts.anchor_col (e.g. to line up under a winbar segment).
  local col = opts.anchor_col
  if not col then
    local win_pos = vim.api.nvim_win_get_position(origin_win)
    col = (win_pos[2] or 0) + math.max(0, vim.fn.wincol() - 1)
  end

  local frame = {
    items      = items,
    on_pick    = opts.on_pick,
    title      = opts.title,
    anchor_col = col,
    drill      = opts.drill,
  }
  stack[#stack + 1] = frame
  render_frame(frame)
end

-- Public: open a nested menu on top of the current one (drill-down).
function M.push(opts)
  local parent = top()
  if not parent then return M.open(opts.items, opts) end
  local new_col = (parent.anchor_col or 0) + vim.api.nvim_win_get_width(parent.winid) + 2
  local frame = {
    items      = opts.items,
    on_pick    = opts.on_pick,
    title      = opts.title,
    anchor_col = new_col,
    drill      = opts.drill,
  }
  stack[#stack + 1] = frame
  render_frame(frame)
end

return M
