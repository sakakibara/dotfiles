-- lua/config/autocmds.lua
local au = vim.api.nvim_create_autocmd
local grp = vim.api.nvim_create_augroup("Lib", { clear = true })

-- Replace Nvim 0.12's default SwapExists handler (nvim.swapfile augroup).
-- Improvements over the stock handler:
--   - Deletes swapfiles whose creator PID is no longer a live nvim process.
--     Checking `kill(pid, 0) == 0` isn't enough: PIDs get recycled, so an
--     unrelated process can occupy the old nvim's slot and still answer
--     the signal. We also match the process name via `ps` to confirm it's
--     actually nvim.
--   - For live-owned swaps, W325 goes through nvim_echo (for :messages)
--     AND vim.notify (for the snacks toast), since snacks.notifier
--     replaces vim.notify and doesn't back-fill :messages.

-- Returns (bool, reason) describing whether <pid> is a live nvim.
-- Live = kill(pid, 0) succeeds, process name ends with "nvim", AND the
-- process is not a zombie (macOS terminals can leave defunct children
-- behind when the parent shell closes, and kill(pid, 0) still succeeds
-- for zombies).
local function pid_is_live_nvim(pid)
  local alive = vim.uv.kill(pid, 0) == 0
  if not alive then return false, "dead (ESRCH)" end
  local r = vim.system({ "ps", "-p", tostring(pid), "-o", "stat=,comm=" }, { text = true }):wait()
  if r.code ~= 0 then return false, "ps failed: " .. (r.stderr or "") end
  local out = (r.stdout or ""):gsub("^%s+", ""):gsub("%s+$", "")
  if out == "" then return false, "ps returned empty" end
  local stat, comm = out:match("^(%S+)%s+(.+)$")
  if not stat or not comm then return false, "ps output unparseable: " .. out end
  if stat:find("Z") then return false, "zombie ("..comm..")" end
  local is_nvim = comm == "nvim" or comm:match("/nvim$") ~= nil
  return is_nvim, comm
end


vim.api.nvim_create_augroup("nvim.swapfile", { clear = true })
au("SwapExists", {
  group = "nvim.swapfile",
  callback = function()
    local info = vim.fn.swapinfo(vim.v.swapname)
    local user = vim.uv.os_get_passwd().username
    local iswin = vim.fn.has("win32") == 1
    if info.error or info.pid <= 0 or (not iswin and info.user ~= user) then
      vim.v.swapchoice = ""
      return
    end

    local live = pid_is_live_nvim(info.pid)
    if not live then
      pcall(vim.fn.delete, vim.v.swapname)
      vim.v.swapchoice = "e"
      return
    end

    vim.v.swapchoice = "e"
    local msg = ("W325: Ignoring swapfile from Nvim process %d"):format(info.pid)
    vim.api.nvim_echo({ { msg, "WarningMsg" } }, true, {})
    vim.notify(msg, vim.log.levels.WARN)
  end,
})

-- `:SwapDebug <pid>` — prints what our heuristic concludes, so false
-- positives / false negatives can be diagnosed without instrumentation.
vim.api.nvim_create_user_command("SwapDebug", function(opts)
  local pid = tonumber(opts.args)
  if not pid then
    print("usage: :SwapDebug <pid>")
    return
  end
  local live, reason = pid_is_live_nvim(pid)
  print(("pid=%d live_nvim=%s reason=%q"):format(pid, tostring(live), reason or ""))
end, { nargs = 1 })

-- auto-create parent directories on save
au("BufWritePre", {
  group = grp,
  callback = function(event)
    if event.match:match("^%w%w+:[\\/][\\/]") then return end  -- skip scp://, oil:// etc
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
  end,
})

-- visual-mode relative numbers: toggle off in visual for clearer selection
au("ModeChanged", {
  group = grp,
  pattern = { "*:[vV\x16]*", "[vV\x16]*:*" },
  callback = function()
    -- Skip floats (menus, pickers, etc.) — they manage their own UI and
    -- have no business showing line numbers regardless of mode.
    if vim.api.nvim_win_get_config(0).relative ~= "" then return end
    local is_visual = vim.fn.mode():match("[vV\x16]")
    vim.opt_local.relativenumber = not is_visual
  end,
})

-- markdown / json: show formatting
au("FileType", {
  group = grp,
  pattern = { "markdown", "json", "jsonc" },
  callback = function() vim.opt_local.conceallevel = 0 end,
})

-- markdown: visible 80-col guide
au("FileType", {
  group = grp,
  pattern = "markdown",
  callback = function() vim.opt_local.colorcolumn = "80" end,
})

-- WSL /mnt/* volumes: don't fix end-of-line (Windows line endings)
au({ "BufReadPre", "BufNewFile" }, {
  group = grp,
  pattern = "/mnt/*",
  callback = function() vim.opt_local.fixeol = false end,
})

-- highlight yank
au("TextYankPost", {
  group = grp,
  callback = function() vim.highlight.on_yank() end,
})
