local M = {}

function M.yank_relative_path()
  local name = vim.api.nvim_buf_get_name(0)
  local rel = vim.fn.fnamemodify(name, ":.")
  vim.fn.setreg("+", rel)
  vim.notify("`" .. rel .. "`", vim.log.levels.INFO, { title = "Yanked" })
end

function M.yank_full_path()
  local name = vim.api.nvim_buf_get_name(0)
  local abs = vim.fn.fnamemodify(name, ":p")
  vim.fn.setreg("+", abs)
  vim.notify("`" .. abs .. "`", vim.log.levels.INFO, { title = "Yanked" })
end

function M.put_empty_line(above)
  local count = vim.v.count1
  local target = vim.api.nvim_win_get_cursor(0)[1] + (above and -1 or 0)
  local blanks = {}
  for _ = 1, count do blanks[#blanks + 1] = "" end
  vim.api.nvim_buf_set_lines(0, target, target, true, blanks)
end

-- Cycle to the next/prev file in the same directory as the current buffer,
-- with wrap. `direction` is 1 (next) or -1 (prev). Filters to regular files,
-- skips dotfiles and subdirectories. No-op when the current buffer has no
-- file path (scratch, terminal, etc.) or its directory is unreadable.
function M.cycle_sibling(direction)
  local cur = vim.api.nvim_buf_get_name(0)
  if cur == "" then
    vim.notify("cycle_sibling: buffer has no file", vim.log.levels.WARN)
    return
  end
  -- Resolve symlinks so directory listing matches the actual on-disk parent.
  local real = vim.uv.fs_realpath(cur) or cur
  local dir = vim.fs.dirname(real)
  local basename = vim.fs.basename(real)

  local fs = vim.uv.fs_scandir(dir)
  if not fs then
    vim.notify("cycle_sibling: cannot read " .. dir, vim.log.levels.WARN)
    return
  end
  local files = {}
  while true do
    local name, t = vim.uv.fs_scandir_next(fs)
    if not name then break end
    if name:sub(1, 1) ~= "." and (t == "file" or t == "link") then
      files[#files + 1] = name
    end
  end
  if #files <= 1 then return end
  table.sort(files, function(a, b) return a:lower() < b:lower() end)

  local idx
  for i, f in ipairs(files) do
    if f == basename then idx = i; break end
  end
  if not idx then
    -- Current file isn't in the listing (filtered out by hidden/etc.). Land
    -- on the first sibling so the user gets *some* movement.
    idx = 0
  end
  local next_idx = ((idx - 1 + direction) % #files) + 1
  vim.cmd("edit " .. vim.fn.fnameescape(dir .. "/" .. files[next_idx]))
end

-- Filetype picker. Used by the statusline filetype segment's click handler
-- and by the <Leader>ut keymap. Snacks.picker.select gives fuzzy filter over
-- the ~200-entry filetype list; falls back to vim.ui.select if snacks isn't up.
function M.pick_filetype()
  local items = vim.fn.getcompletion("", "filetype")
  table.sort(items)
  local select = (Snacks and Snacks.picker and Snacks.picker.select) or vim.ui.select
  select(items, { prompt = "Set filetype: " }, function(choice)
    if choice and choice ~= "" then vim.bo.filetype = choice end
  end)
end

return M
