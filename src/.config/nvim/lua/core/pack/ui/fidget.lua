local M = {}

local SPINNER = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
local SPINNER_INTERVAL_MS = 100
local FADE_MS = 1500

function M.fidget(opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(false, true)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].swapfile = false
  vim.bo[buf].bufhidden = "wipe"
  if opts.open_window == false then vim.bo[buf].filetype = "PackFidget" end
  pcall(vim.api.nvim_buf_set_name, buf, "core.pack: fidget")

  -- Row state: { name, text, status = "active"|"done"|"error" }, in insertion order.
  local rows = {}
  local row_index = {}
  local spinner_step = 1

  local function render()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    local lines = {}
    for i, r in ipairs(rows) do
      local glyph
      -- Lib.icons.status.* include a baked-in trailing space for inline
      -- decorator use; strip it so the spinner / done / error glyphs all
      -- contribute the same effective width.
      if r.status == "done" then glyph = (Lib.icons.status.Success):gsub("%s+$", "")
      elseif r.status == "error" then glyph = (Lib.icons.status.Failure):gsub("%s+$", "")
      else glyph = SPINNER[((spinner_step - 1) % #SPINNER) + 1]
      end
      lines[i] = (" %s %s  %s"):format(glyph, r.name, r.text or "")
    end
    if #lines == 0 then lines = { "" } end
    vim.bo[buf].modifiable = true
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.bo[buf].modifiable = false
  end

  local win
  local function open_window()
    if win and vim.api.nvim_win_is_valid(win) then return end
    local width = 0
    for _, r in ipairs(rows) do width = math.max(width, #r.name + #(r.text or "") + 6) end
    width = math.max(width, 30)
    win = vim.api.nvim_open_win(buf, false, {
      relative = "editor", anchor = "SE",
      row = vim.o.lines - 1, col = vim.o.columns,
      width = width, height = math.max(#rows, 1),
      style = "minimal", border = "none",
      focusable = false, zindex = 200,
    })
    vim.bo[buf].filetype = "PackFidget"
  end

  local function resize_window()
    if not win or not vim.api.nvim_win_is_valid(win) then return end
    local width = 0
    for _, r in ipairs(rows) do width = math.max(width, #r.name + #(r.text or "") + 6) end
    width = math.max(width, 30)
    vim.api.nvim_win_set_config(win, {
      relative = "editor", anchor = "SE",
      row = vim.o.lines - 1, col = vim.o.columns,
      width = width, height = math.max(#rows, 1),
    })
  end

  local timer
  local function tick()
    if not vim.api.nvim_buf_is_valid(buf) then return end
    spinner_step = spinner_step + 1
    render()
  end

  local function ensure_timer()
    if opts.open_window == false then return end
    if timer then return end
    timer = vim.uv.new_timer()
    timer:start(SPINNER_INTERVAL_MS, SPINNER_INTERVAL_MS, vim.schedule_wrap(tick))
  end

  local view = { buf = buf, win = win }

  function view:set_status(name, text)
    if row_index[name] then
      rows[row_index[name]].text = text
      rows[row_index[name]].status = "active"
    else
      rows[#rows + 1] = { name = name, text = text, status = "active" }
      row_index[name] = #rows
    end
    render()
    if opts.open_window ~= false then
      open_window()
      resize_window()
      ensure_timer()
    end
  end

  function view:done(name)
    if not row_index[name] then return end
    rows[row_index[name]].status = "done"
    rows[row_index[name]].text = ""
    render()
    -- Schedule fade-out for this row.
    vim.defer_fn(function()
      if not vim.api.nvim_buf_is_valid(buf) then return end
      local idx = row_index[name]
      if not idx then return end
      table.remove(rows, idx)
      row_index[name] = nil
      -- Reindex.
      for i, r in ipairs(rows) do row_index[r.name] = i end
      if #rows == 0 then
        view:close()
      else
        render(); resize_window()
      end
    end, FADE_MS)
  end

  function view:error(name, text)
    if not row_index[name] then
      rows[#rows + 1] = { name = name, text = text, status = "error" }
      row_index[name] = #rows
    else
      rows[row_index[name]].status = "error"
      rows[row_index[name]].text = text
    end
    render()
  end

  function view:close()
    if timer then timer:stop(); timer:close(); timer = nil end
    if win and vim.api.nvim_win_is_valid(win) then
      vim.api.nvim_win_close(win, true)
      win = nil
    end
    if vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_delete(buf, { force = true })
    end
  end

  return view
end

return M
