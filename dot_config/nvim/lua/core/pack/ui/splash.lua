local M = {}

-- Cold-install splash. Shown only when pack.setup is blocking on a fresh
-- install (#to_install > 0 in install_all). A full-screen floating buffer
-- with a centered, themed box — replaces the otherwise-blank screen and
-- all half-rendered chrome (statusline, winbar, tabline) during the
-- cold-start vim.wait.
function M.cold_install_splash(total)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype   = "nofile"
  vim.bo[buf].swapfile  = false
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].filetype  = "PackSplash"
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: install")

  -- Save every piece of chrome we hide so :close() restores cleanly.
  -- cmdheight is set high (not 0) — a 0-height cmdline guarantees the
  -- press-enter prompt for any message, and that prompt blocks the
  -- main thread so vim.defer_fn / vim.on_key can't fire to close the
  -- splash. Splash floats over everything, so the visually-large
  -- cmdline area is hidden anyway. shortmess "aT" truncates verbose
  -- messages to a single line for safety.
  local saved = {
    laststatus  = vim.o.laststatus,
    showtabline = vim.o.showtabline,
    winbar      = vim.o.winbar,
    statusline  = vim.o.statusline,
    tabline     = vim.o.tabline,
    cmdheight   = vim.o.cmdheight,
    more        = vim.o.more,
    shortmess   = vim.o.shortmess,
    ruler       = vim.o.ruler,
    showcmd     = vim.o.showcmd,
    cursor_hl   = vim.api.nvim_get_hl(0, { name = "Cursor" }),
    lcursor_hl  = vim.api.nvim_get_hl(0, { name = "lCursor" }),
  }
  vim.o.laststatus  = 0
  vim.o.showtabline = 0
  vim.o.winbar      = ""
  -- ruler shows "L,C  Pct" in the cmdline strip when there is no
  -- statusline (laststatus=0). It looks like a status bar to the user;
  -- suppress.
  vim.o.ruler       = false
  vim.o.showcmd     = false
  -- Some redraw paths still evaluate the `statusline` / `tabline`
  -- expressions even with laststatus=0/showtabline=0; clearing the
  -- expression strings makes the suppression unconditional.
  vim.o.statusline  = ""
  vim.o.tabline     = ""

  -- Lock chrome options against re-set. OptionSet autocmd is unreliable
  -- here because some plugins set the option AFTER triggering the redraw,
  -- so even though we snap back to 0 the screen renders the intermediate
  -- value. Brute-force: a libuv timer re-asserts every 50ms while splash
  -- is up. The timer runs off the main thread but its callback runs on
  -- main; with main blocked for long stretches by install work, the
  -- callbacks queue and drain in order, eventually catching up. The
  -- timer is stopped on :close().
  local lock_timer = vim.uv.new_timer()
  lock_timer:start(0, 50, vim.schedule_wrap(function()
    if vim.o.laststatus  ~= 0   then vim.o.laststatus  = 0   end
    if vim.o.showtabline ~= 0   then vim.o.showtabline = 0   end
    if vim.o.winbar      ~= ""  then vim.o.winbar      = ""  end
    if vim.o.statusline  ~= ""  then vim.o.statusline  = ""  end
    if vim.o.tabline     ~= ""  then vim.o.tabline     = ""  end
    if vim.o.ruler                then vim.o.ruler     = false end
    if vim.o.showcmd              then vim.o.showcmd   = false end
  end))
  -- cmdheight = 1 (not 0 or large): 0 guarantees press-enter on every
  -- message and the prompt blocks the main thread; large cmdheight just
  -- shrinks the floating-window editor area, leaving the cmdline visible
  -- below the splash. 1 lets each truncated message slot in cleanly with
  -- no hit-enter, and the cmdline strip at the bottom of the screen is
  -- usually unobtrusive against the splash. shortmess "aT" forces
  -- single-line truncation so multi-line messages can't overflow.
  vim.o.cmdheight   = 1
  vim.o.more        = false
  vim.opt.shortmess:append("aT")
  -- Hide the terminal cursor via DECTCEM (CSI ?25 l). The TUI cursor is
  -- ultimately rendered by the terminal, not nvim, so highlight tweaks
  -- and `guicursor` settings only affect shape/color — not visibility.
  -- The escape sequence directly tells the terminal to hide its cursor.
  -- Restored with CSI ?25 h on :close().
  pcall(io.stdout.write, io.stdout, "\27[?25l")
  pcall(io.stdout.flush, io.stdout)

  local SCREEN_W = vim.o.columns
  local SCREEN_H = vim.o.lines  -- splash floats over the cmdline area too
  local BOX_W    = 53                              -- includes borders + gutter
  local BOX_H    = 9                               -- 7 content rows + 2 borders
  local BAR      = 25                              -- progress-bar cells
  local GUTTER   = 2                                -- padding between border and text
  local pad_top  = math.floor((SCREEN_H - BOX_H) / 2)
  local pad_left = math.floor((SCREEN_W - BOX_W) / 2)
  local inner_w  = BOX_W - 2                        -- between the side borders
  local text_w   = inner_w - GUTTER * 2              -- usable text area
  local done     = 0

  -- Phase: "install" while clones+builds are running, "setup" while
  -- eager-load configs run after install_all completes. Setup phase
  -- shows a (loaded/total) counter + per-plugin name + an animated
  -- spinner. The spinner is driven by a libuv timer that fires
  -- ~10/sec, independent of the main thread, so it keeps moving even
  -- when no new messages are arriving (e.g., between treesitter
  -- compile stages). A stuck spinner means a real main-thread block;
  -- a moving spinner means the editor is alive even though the work
  -- behind the scenes is silent.
  local phase           = "install"
  local setup_status    = ""
  local spinner_step    = 1
  local SPINNER         = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
  local spinner_timer   = nil

  local ns = vim.api.nvim_create_namespace("PackSplash")

  -- Build a centered line of `inner_w` display cells, with `GUTTER` cells
  -- of padding on each side (so text never butts up against the border).
  -- Text is centered within the text_w region between the gutters.
  local function center(text)
    local w    = vim.fn.strdisplaywidth(text)
    local left = math.floor((text_w - w) / 2)
    if left < 0 then left = 0 end
    local centered = (" "):rep(left) .. text
    local pad_right = inner_w - GUTTER - vim.fn.strdisplaywidth(centered)
    if pad_right < GUTTER then pad_right = GUTTER end
    return (" "):rep(GUTTER) .. centered .. (" "):rep(pad_right)
  end

  -- Compose the box content. Returns the buffer lines and a list of
  -- {row, col_start_byte, col_end_byte, hl_group} highlight ranges.
  local function compose()
    local title    = phase == "install"
      and "core.pack · downloading plugins"
      or  "core.pack · loading plugins"

    -- Middle row content depends on phase:
    --   install: progress bar with N/M counter
    --   setup:   spinner + currently-loading plugin name
    local mid_line, prog_pad_left, filled, count_w
    if phase == "install" then
      filled = (total > 0) and math.floor(done * BAR / total) or 0
      local bar_filled = ("▰"):rep(filled)
      local bar_empty  = ("▱"):rep(BAR - filled)
      -- Pad `done` digits to the width of `total`, so the count column
      -- has constant width regardless of how far we've progressed —
      -- otherwise prog_pad_left changes when `done` crosses 10/100,
      -- visibly shifting the entire bar by one cell every decade.
      local digits = #tostring(total)
      local count = ("%" .. digits .. "d/%d"):format(done, total)
      local bar_w = BAR * vim.fn.strdisplaywidth("▰")
      count_w     = vim.fn.strdisplaywidth(count)
      local prog_text  = bar_filled .. bar_empty .. "  " .. count
      -- Build the prog row: GUTTER + center-pad + prog_text + fill to inner_w.
      prog_pad_left    = math.floor((text_w - bar_w - 2 - count_w) / 2)
      if prog_pad_left < 0 then prog_pad_left = 0 end
      mid_line = (" "):rep(GUTTER) .. (" "):rep(prog_pad_left) .. prog_text
      -- Pad to exactly inner_w cells. The gutter is already accounted
      -- for in the leading spaces, so we just fill to inner_w total.
      local pad_right = inner_w - vim.fn.strdisplaywidth(mid_line)
      if pad_right < 0 then pad_right = 0 end
      mid_line = mid_line .. (" "):rep(pad_right)
      -- prog_pad_left for highlight calc: byte offset from inner start.
      prog_pad_left = GUTTER + prog_pad_left
    else
      local spin = SPINNER[((spinner_step - 1) % #SPINNER) + 1]
      mid_line = center(spin .. "  " .. setup_status)
    end

    local box_rows = {
      "╭" .. ("─"):rep(BOX_W - 2) .. "╮",
      "│" .. center("")                         .. "│",
      "│" .. center(title)                      .. "│",
      "│" .. center("")                         .. "│",
      "│" .. mid_line                           .. "│",
      "│" .. center("")                         .. "│",
      "│" .. center("one-time setup — restart isn't needed") .. "│",
      "│" .. center("")                         .. "│",
      "╰" .. ("─"):rep(BOX_W - 2) .. "╯",
    }

    -- Full screen: empty top padding + indented box rows + empty bottom.
    local lines = {}
    for _ = 1, pad_top do lines[#lines + 1] = "" end
    for _, row in ipairs(box_rows) do
      lines[#lines + 1] = (" "):rep(pad_left) .. row
    end
    while #lines < SCREEN_H do lines[#lines + 1] = "" end

    -- Highlight ranges. Each line is `pad_left ASCII spaces + box_row`.
    -- Box glyphs are 3 UTF-8 bytes each, so the side borders ("│") are
    -- exactly 3 bytes. Per-line:
    --   left border  : [pad_left, pad_left+3)
    --   inner content: [pad_left+3, #line-3)
    --   right border : [#line-3, #line)
    -- Highlights for inner-row content (title, subtitle, progress) MUST
    -- exclude both borders so FloatBorder isn't visually overridden by
    -- dimmer groups like Comment.
    local hls = {}
    local box_top    = pad_top
    local box_bottom = pad_top + BOX_H - 1

    local function add(row, sb, eb, hl)
      hls[#hls + 1] = { row = row, sb = sb, eb = eb, hl = hl }
    end

    -- Per-row inner range. Each box row has predictable left/right
    -- border byte offsets, so we read them from the assembled line
    -- rather than recomputing display widths.
    local function inner_range(row_idx)
      local line = lines[row_idx + 1]  -- 1-indexed
      return pad_left + 3, #line - 3
    end

    -- Top and bottom border rows are entirely FloatBorder.
    add(box_top,    pad_left, -1, "FloatBorder")
    add(box_bottom, pad_left, -1, "FloatBorder")
    -- Side borders on each interior row, both edges.
    for r = box_top + 1, box_bottom - 1 do
      local line = lines[r + 1]
      add(r, pad_left,        pad_left + 3, "FloatBorder")
      add(r, #line - 3,       #line,        "FloatBorder")
    end

    -- Inner content highlights. Title row = box_top + 2, progress row
    -- = box_top + 4, subtitle row = box_top + 6.
    do
      local sb, eb = inner_range(box_top + 2)
      add(box_top + 2, sb, eb, "Title")
    end
    do
      local sb, eb = inner_range(box_top + 6)
      add(box_top + 6, sb, eb, "Comment")
    end

    if phase == "install" then
      -- Progress segments: filled (String), empty (Comment), N/M (Constant).
      -- Byte offsets are deterministic because the progress line is
      -- ASCII spaces + N×▰ + M×▱ + "  " + ASCII count.
      local prog_row    = box_top + 4
      local prog_inner  = pad_left + 3  -- skip left "│"
      local filled_sb   = prog_inner + prog_pad_left
      local filled_eb   = filled_sb + filled * 3
      local empty_eb    = filled_eb + (BAR - filled) * 3
      local count_sb    = empty_eb + 2  -- skip the 2 ASCII spaces
      add(prog_row, filled_sb, filled_eb,         "String")
      add(prog_row, filled_eb, empty_eb,          "Comment")
      add(prog_row, count_sb,  count_sb + count_w, "Constant")
    else
      -- Setup phase: highlight the leading spinner glyph as Title (the
      -- attention-grabbing element) and any "(N/M)" counter that
      -- follows as Constant.
      local line = lines[box_top + 5]  -- 1-indexed
      local lead_spaces = #(line:sub(pad_left + 4):match("^ *") or "")
      local spin_sb = pad_left + 3 + lead_spaces
      add(box_top + 4, spin_sb, spin_sb + 3, "Title")  -- ⠋ etc. are 3-byte
      local counter_match = line:sub(spin_sb + 6):match("^(%([%d/]+%))")
      if counter_match then
        local counter_sb = spin_sb + 5  -- past spinner + 2 spaces
        add(box_top + 4, counter_sb, counter_sb + #counter_match, "Constant")
      end
    end

    return lines, hls
  end

  local function render()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    local lines, hls = compose()
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modifiable = false
    vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)
    for _, h in ipairs(hls) do
      pcall(vim.api.nvim_buf_add_highlight, buf, ns, h.hl, h.row, h.sb, h.eb)
    end
  end

  render()

  -- enter = true so the cursor lives in the splash window. Combined with
  -- the Cursor highlight override above (fg = bg = NormalFloat.bg), the
  -- cursor block renders invisibly against the splash background. Without
  -- entering, the cursor stays in the [No Name] background window where
  -- the highlight tweak doesn't help.
  local win = vim.api.nvim_open_win(buf, true, {
    relative  = "editor",
    width     = SCREEN_W,
    height    = SCREEN_H,
    row       = 0,
    col       = 0,
    style     = "minimal",
    border    = "none",
    focusable = false,
    zindex    = 100,
  })
  -- Use the float palette so the splash visually distinguishes from
  -- whatever buffer/wincolor scheme is otherwise active.
  vim.wo[win].winhighlight = "Normal:NormalFloat"

  local view = { buf = buf, win = win }

  -- Publish to the public UI module so external callers (treesitter logger
  -- override, blink download notifier) can pipe progress text into us.
  -- Lazy require avoids a cyclic load: we are required from ui/init.lua,
  -- and writing into UI.* happens here at call time, after init.lua finishes.
  local UI = require("core.pack.ui")
  UI._active_splash = view

  -- Idle close: splash auto-closes after `idle_close_ms` of no status
  -- updates. Reset on every set_setup_status / set_status_text call,
  -- so as long as something keeps updating us we stay open. The timer
  -- is also explicit-cancellable on close.
  local close_timer    = nil
  local idle_close_ms  = nil

  local function bump_idle()
    if not idle_close_ms then return end
    if close_timer then
      pcall(close_timer.stop, close_timer)
      pcall(close_timer.close, close_timer)
    end
    close_timer = vim.uv.new_timer()
    close_timer:start(idle_close_ms, 0, vim.schedule_wrap(function() view:close() end))
  end

  -- Truncate `text` to fit in `max_w` display columns; adds an ellipsis
  -- when truncated. Char-aware via strcharpart so multi-byte input
  -- doesn't get cut mid-codepoint.
  local function truncate(text, max_w)
    if vim.fn.strdisplaywidth(text) <= max_w then return text end
    local n = max_w - 1
    while n > 0 and vim.fn.strdisplaywidth(vim.fn.strcharpart(text, 0, n) .. "…") > max_w do
      n = n - 1
    end
    return vim.fn.strcharpart(text, 0, math.max(n, 0)) .. "…"
  end

  function view:update(d)
    done = d or done
    render()
    -- Force redraw so progress is visible even though setup() is blocked
    -- in vim.wait. vim.wait pumps the event loop but doesn't issue a
    -- draw on its own.
    pcall(vim.cmd.redraw)
  end

  -- Switch the splash to setup phase: progress bar replaced with spinner +
  -- per-plugin status. Called from the coordinator after install_all
  -- completes, before eager loads start.
  function view:enter_setup_phase()
    phase = "setup"
    render()
    pcall(vim.cmd.redraw)
    -- Start the spinner ticker. Fires every 100ms via libuv, advances
    -- the spinner one frame, schedules a redraw on the main thread.
    -- The libuv timer ticks regardless of Lua state, so the spinner
    -- keeps animating even when no status messages are arriving.
    if not spinner_timer then
      spinner_timer = vim.uv.new_timer()
      spinner_timer:start(100, 100, vim.schedule_wrap(function()
        if not vim.api.nvim_buf_is_valid(buf) then return end
        spinner_step = spinner_step + 1
        render()
        pcall(vim.cmd.redraw)
      end))
    end
  end

  -- Update the per-plugin status shown in the setup phase: "(idx/total)
  -- name". Each call forces a redraw so the user sees the counter
  -- advance even though the surrounding loop runs synchronously.
  function view:set_setup_status(idx, total_eagers, name)
    setup_status = ("(%d/%d)  %s"):format(idx or 0, total_eagers or 0, name or "")
    render()
    pcall(vim.cmd.redraw)
    bump_idle()
  end

  -- Set the middle row to arbitrary text (truncated to fit). Used by
  -- external callers (e.g., the nvim-treesitter logger override) to pipe
  -- their progress directly into the splash instead of going through
  -- nvim_echo, which would overflow the cmdline and trigger press-enter.
  function view:set_status_text(text)
    -- Account for the spinner glyph + 2 spaces that prefix the text in
    -- setup-phase rendering: text_w - 3 (spinner is 1 cell, 2 spaces)
    setup_status = truncate(text or "", text_w - 3)
    render()
    pcall(vim.cmd.redraw)
    bump_idle()
  end

  -- Start the idle-close timer. Splash will close `ms` milliseconds
  -- after the most recent status update. Each subsequent update bumps
  -- the deadline forward, so the splash stays open as long as something
  -- is reporting progress.
  function view:start_idle_close(ms)
    idle_close_ms = ms
    bump_idle()
  end

  function view:close()
    if spinner_timer then
      pcall(spinner_timer.stop, spinner_timer)
      pcall(spinner_timer.close, spinner_timer)
      spinner_timer = nil
    end
    if close_timer then
      pcall(close_timer.stop, close_timer)
      pcall(close_timer.close, close_timer)
      close_timer = nil
    end
    -- Release the chrome-option lock timer.
    if lock_timer then
      pcall(lock_timer.stop, lock_timer)
      pcall(lock_timer.close, lock_timer)
      lock_timer = nil
    end
    if win and vim.api.nvim_win_is_valid(win) then
      pcall(vim.api.nvim_win_close, win, true)
    end
    if vim.api.nvim_buf_is_valid(buf) then
      pcall(vim.api.nvim_buf_delete, buf, { force = true })
    end
    -- Restore the simple option saves.
    for _, k in ipairs({ "laststatus", "showtabline", "winbar", "statusline", "tabline", "cmdheight", "more", "shortmess", "ruler", "showcmd" }) do
      vim.o[k] = saved[k]
    end
    -- Restore Cursor highlights (the saved hl table contains all original
    -- attributes; passing the table reinstates them as-is).
    pcall(vim.api.nvim_set_hl, 0, "Cursor",  saved.cursor_hl)
    pcall(vim.api.nvim_set_hl, 0, "lCursor", saved.lcursor_hl)
    -- Show the terminal cursor again (DECTCEM CSI ?25 h), undoing the
    -- hide we sent on splash open.
    pcall(io.stdout.write, io.stdout, "\27[?25h")
    pcall(io.stdout.flush, io.stdout)
    -- Force a full redraw + chrome re-derivation so the restored
    -- statusline / tabline / winbar render with correct highlights
    -- and segment separators. The custom Lib.statusline derives
    -- segment highlights and caches per-buffer rendered text; without
    -- explicit invalidation the first post-splash redraw can paint
    -- with stale separator colors. Firing a ColorScheme event
    -- triggers Lib.statusline's own ColorScheme autocmd which
    -- re-defines highlights from scratch.
    pcall(vim.api.nvim_exec_autocmds, "ColorScheme", { modeline = false })
    pcall(vim.cmd, "redrawstatus!")
    pcall(vim.cmd, "redrawtabline")
    pcall(vim.cmd, "redraw!")
    if UI._active_splash == self then UI._active_splash = nil end
  end

  return view
end

return M
