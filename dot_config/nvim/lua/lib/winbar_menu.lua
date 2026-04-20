-- lua/lib/winbar_menu.lua
--
-- Linked-list menu architecture (independently implemented, inspired by
-- the state-machine dropbar.nvim uses).
--
-- Each menu is a self-contained instance carrying its own window / buffer
-- and pointers to parent (prev_menu) and child (sub_menu). A global
-- registry `menus[winid] = instance` is the single source of truth for
-- "is this float one of ours?". Closing is idempotent + recursive:
-- :close() tears down the sub_menu first, then itself, and restores
-- focus to prev_win. No shared stack, no belt-and-braces iteration.

local M = {}

local menus = {}
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

-- ─────────────────────────────────────────────────────────────────────
-- Menu instance
-- ─────────────────────────────────────────────────────────────────────
local Menu = {}
Menu.__index = Menu

function Menu.new(opts)
  local self = setmetatable({}, Menu)
  self.items      = opts.items
  self.title      = opts.title
  self.on_pick    = opts.on_pick
  self.drill      = opts.drill
  self.anchor_col = opts.anchor_col or 0
  self.prev_win   = opts.prev_win  or vim.api.nvim_get_current_win()
  self.prev_menu  = menus[self.prev_win]
  if self.prev_menu then
    if self.prev_menu.sub_menu and self.prev_menu.sub_menu ~= self then
      self.prev_menu.sub_menu:close()
    end
    self.prev_menu.sub_menu = self
  end
  self.is_opened = false
  self.sel       = 1
  return self
end

function Menu:_build_lines()
  local lines, hlspecs = {}, {}
  local widest = 0
  for i, it in ipairs(self.items) do
    local marker   = it.current and "● " or "  "
    local icon     = (it.icon or "") ~= "" and it.icon or ""
    local icon_pad = icon ~= "" and (icon .. " ") or ""
    local label    = it.label or ""
    local line     = " " .. marker .. icon_pad .. label .. " "
    lines[i] = line

    local col   = 1
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
  -- Pad one cell past the widest line so the float can size width+1
  -- (gives the drag-cursor a safe EOL cell; see :open()).
  for i, line in ipairs(lines) do
    local pad = widest + 1 - vim.fn.strdisplaywidth(line)
    if pad > 0 then lines[i] = line .. string.rep(" ", pad) end
  end
  return lines, widest, hlspecs
end

function Menu:_apply_hlspecs(hlspecs)
  vim.api.nvim_buf_clear_namespace(self.buf, ns, 0, -1)
  for row, specs in ipairs(hlspecs) do
    for _, s in ipairs(specs) do
      vim.api.nvim_buf_set_extmark(self.buf, ns, row - 1, s[1], {
        end_row = row - 1, end_col = s[2], hl_group = s[3], priority = 50,
      })
    end
  end
end

function Menu:_apply_sel(idx)
  local line_count = vim.api.nvim_buf_line_count(self.buf)
  local end_row = math.min(idx, line_count)
  vim.api.nvim_buf_set_extmark(self.buf, ns, idx - 1, 0, {
    end_row = end_row, end_col = 0,
    hl_group = "LibWinbarMenuSel",
    hl_eol = true, priority = 100,
  })
  if self.win and vim.api.nvim_win_is_valid(self.win) then
    pcall(vim.api.nvim_win_set_cursor, self.win, { idx, 0 })
  end
end

function Menu:select(idx)
  self.sel = idx
  self:_apply_hlspecs(self._hlspecs)
  self:_apply_sel(idx)
end

function Menu:move(delta)
  local new = self.sel + delta
  if new < 1 then new = #self.items end
  if new > #self.items then new = 1 end
  self:select(new)
end

function Menu:open()
  if self.is_opened then return end
  set_hl()

  local lines, widest, hlspecs = self:_build_lines()
  self._hlspecs = hlspecs

  self.buf = vim.api.nvim_create_buf(false, true)
  vim.bo[self.buf].bufhidden = "wipe"
  vim.bo[self.buf].buftype   = "nofile"
  vim.bo[self.buf].filetype  = "libwinbarmenu"
  vim.api.nvim_buf_set_lines(self.buf, 0, -1, false, lines)
  vim.bo[self.buf].modifiable = false

  local max_h = math.min(#lines, math.max(6, math.floor(vim.o.lines * 0.4)))
  -- One extra cell so visual-mode cursor at EOL (exclusive selection)
  -- sits on blank padding instead of the window edge — avoids the
  -- horizontal scroll nudge during mouse drag-select.
  local width  = widest + 1
  local anchor = math.min(self.anchor_col, math.max(0, vim.o.columns - width - 2))

  self.win = vim.api.nvim_open_win(self.buf, true, {
    relative  = "editor",
    row       = 1,
    col       = anchor,
    width     = width,
    height    = max_h,
    style     = "minimal",
    border    = "rounded",
    title     = self.title and (" " .. self.title .. " ") or nil,
    title_pos = "left",
    zindex    = 250,
  })
  vim.wo[self.win].winhl         = "Normal:LibWinbarMenu,NormalFloat:LibWinbarMenu,FloatBorder:LibWinbarMenuBorder,CursorLine:LibWinbarMenuSel"
  vim.wo[self.win].cursorline    = true
  vim.wo[self.win].wrap          = false
  vim.wo[self.win].number        = false
  vim.wo[self.win].relativenumber= false
  vim.wo[self.win].sidescrolloff = 0
  -- style=minimal clears most window-local UI but leaves statuscolumn
  -- and signcolumn alone, so our global Lib.statuscolumn render leaks
  -- into the float. Explicitly disable.
  vim.wo[self.win].statuscolumn  = ""
  vim.wo[self.win].signcolumn    = "no"
  vim.wo[self.win].foldcolumn    = "0"
  vim.cmd.stopinsert()

  self.is_opened = true
  menus[self.win] = self

  local initial = 1
  for i, it in ipairs(self.items) do
    if it.current then initial = i; break end
  end
  self:select(initial)

  self:_wire_keymaps()
  self:_wire_autocmds()
end

function Menu:root()
  local r = self
  while r.prev_menu do r = r.prev_menu end
  return r
end

-- Idempotent + recursive close. Submenu closes first (child-first teardown),
-- then self. Focus returns to prev_win. Safe to call multiple times or
-- during an autocmd firing on the same window.
function Menu:close()
  if not self.is_opened then return end
  self.is_opened = false
  if self.sub_menu then
    self.sub_menu:close()
    self.sub_menu = nil
  end
  if self.prev_menu then
    self.prev_menu.sub_menu = nil
  end
  if self.win and vim.api.nvim_win_is_valid(self.win) then
    menus[self.win] = nil
    pcall(vim.api.nvim_win_close, self.win, true)
  elseif self.win then
    menus[self.win] = nil
  end
  if self.buf and vim.api.nvim_buf_is_valid(self.buf) then
    pcall(vim.api.nvim_buf_delete, self.buf, { force = true })
  end
  if self.prev_win and vim.api.nvim_win_is_valid(self.prev_win) then
    pcall(vim.api.nvim_set_current_win, self.prev_win)
  end
end

function Menu:pick()
  local item = self.items[self.sel]
  if not item then return end
  if item.is_dir and self.drill then
    local children = self.drill(item)
    if children and #children > 0 then
      local new_col = self.anchor_col + (vim.api.nvim_win_is_valid(self.win)
        and vim.api.nvim_win_get_width(self.win) or 0) + 2
      local child = Menu.new({
        items      = children,
        on_pick    = self.on_pick,
        drill      = self.drill,
        title      = item.label,
        anchor_col = new_col,
        prev_win   = self.win,
      })
      child:open()
      return
    end
  end
  local on_pick = self.on_pick
  self:root():close()
  if on_pick then on_pick(item) end
end

function Menu:_wire_keymaps()
  local function map(lhs, fn)
    vim.keymap.set({ "n", "i", "v" }, lhs, function()
      vim.cmd.stopinsert(); fn()
    end, { buffer = self.buf, silent = true, nowait = true })
  end
  map("j",             function() self:move(1)  end)
  map("k",             function() self:move(-1) end)
  map("<Down>",        function() self:move(1)  end)
  map("<Up>",          function() self:move(-1) end)
  map("<C-d>",         function() self:move(5)  end)
  map("<C-u>",         function() self:move(-5) end)
  map("g",             function() self:select(1)               end)
  map("G",             function() self:select(#self.items)     end)
  map("l",             function() self:pick() end)
  map("<Right>",       function() self:pick() end)
  map("<CR>",          function() self:pick() end)
  map("<2-LeftMouse>", function() self:pick() end)
  -- No <LeftMouse> binding: we want default vim behavior so clicks
  -- OUTSIDE the menu correctly change focus and fire BufLeave, and
  -- inside-drags can still select for copy. CursorMoved below keeps
  -- self.sel in sync with where the cursor lands.
  map("h",     function() if self.prev_menu then self:close() else self:root():close() end end)
  map("<Left>",function() if self.prev_menu then self:close() else self:root():close() end end)
  map("q",     function() self:root():close() end)
  map("<Esc>", function() self:root():close() end)
end

function Menu:_wire_autocmds()
  local group = vim.api.nvim_create_augroup("Lib.winbar_menu." .. self.win, { clear = true })

  vim.api.nvim_create_autocmd("WinClosed", {
    group = group, pattern = tostring(self.win), once = true,
    callback = function()
      -- Window is closing — tear down self and child without trying to
      -- re-close the already-closing window.
      if not self.is_opened then return end
      self.is_opened = false
      if self.sub_menu then self.sub_menu:close() end
      if self.prev_menu then self.prev_menu.sub_menu = nil end
      menus[self.win] = nil
      if self.prev_win and vim.api.nvim_win_is_valid(self.prev_win) then
        pcall(vim.api.nvim_set_current_win, self.prev_win)
      end
    end,
  })

  vim.api.nvim_create_autocmd("BufLeave", {
    group = group, buffer = self.buf,
    callback = function()
      vim.schedule(function()
        if not self.is_opened then return end
        local cur = vim.api.nvim_get_current_buf()
        if vim.api.nvim_buf_is_valid(cur) and vim.bo[cur].filetype ~= "libwinbarmenu" then
          self:root():close()
        end
      end)
    end,
  })

  -- Keep self.sel in sync with cursor line (updated by mouse click —
  -- default vim behavior — or arrow keys / j/k). Guard so our own
  -- :select() calls don't loop.
  vim.api.nvim_create_autocmd("CursorMoved", {
    group = group, buffer = self.buf,
    callback = function()
      if not self.is_opened then return end
      local line = vim.api.nvim_win_get_cursor(self.win)[1]
      if line ~= self.sel then
        self.sel = line
        self:_apply_hlspecs(self._hlspecs)
        self:_apply_sel(line)
      end
    end,
  })
end

-- ─────────────────────────────────────────────────────────────────────
-- Public API — keeps the signatures callers expect
-- ─────────────────────────────────────────────────────────────────────
function M.open(items, opts)
  if not items or #items == 0 then return end
  opts = opts or {}
  local prev_win = opts.original_win or vim.api.nvim_get_current_win()

  -- If the click came from a menu (drill-down path), Menu.new() links in
  -- as a child. Otherwise it's a top-level open from a real source
  -- window; close any lingering menu tree first.
  if not menus[prev_win] then
    for _, m in pairs(menus) do
      m:root():close()
      break
    end
  end

  Menu.new({
    items      = items,
    on_pick    = opts.on_pick,
    drill      = opts.drill,
    title      = opts.title,
    anchor_col = opts.anchor_col or 0,
    prev_win   = prev_win,
  }):open()
end

function M.close_all()
  -- Collect snapshot first: close() mutates `menus`, so iterating it
  -- directly would skip entries. Close every instance that's still open,
  -- in case a disjoint tree exists.
  local snapshot = {}
  for _, m in pairs(menus) do snapshot[#snapshot + 1] = m end
  for _, m in ipairs(snapshot) do
    if m.is_opened then m:close() end
  end
end

return M
