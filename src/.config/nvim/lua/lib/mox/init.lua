local M = {}

function M.repo()
  local r = vim.env.MOX_REPO
  if r and r ~= "" then return (r:gsub("/+$", "")) end
  return vim.fn.expand("~/.local/share/mox/dotfiles")
end

function M.defaults()
  return { src_root = M.repo() .. "/src", home = vim.fn.expand("~") }
end

function M.live_of(src_path, o)
  o = o or M.defaults()
  local prefix = o.src_root .. "/"
  if src_path:sub(1, #prefix) ~= prefix then return nil end
  local rel = src_path:sub(#prefix + 1)
  local parts = vim.split(rel, "/", { plain = true })
  local kept = {}
  for _, part in ipairs(parts) do
    if part:match("%.d$") and part ~= ".d" then
      table.insert(kept, (part:gsub("%.d$", "")))
      break
    end
    table.insert(kept, part)
  end
  return o.home .. "/" .. table.concat(kept, "/")
end

function M.source_of(live_path, o)
  o = o or M.defaults()
  local prefix = o.home .. "/"
  if live_path:sub(1, #prefix) ~= prefix then return nil end
  return o.src_root .. "/" .. live_path:sub(#prefix + 1)
end

local function buf_path()
  return vim.api.nvim_buf_get_name(0)
end

local function live_target_of_current()
  local name = buf_path()
  local live = M.live_of(name)
  if live then return live end
  local src = M.source_of(name)
  if src and vim.uv.fs_stat(src) then return name end
  return nil
end

local function notify_result(verb, out)
  if out.code == 0 then return end
  local text = (out.stderr or "") .. (out.stdout or "")
  local tail = {}
  for line in text:gmatch("[^\n]+") do
    table.insert(tail, line)
    if #tail > 4 then table.remove(tail, 1) end
  end
  vim.notify(("mox %s: %s"):format(verb, table.concat(tail, "\n")), vim.log.levels.WARN)
end

local function run_scoped(verb, target, on_done)
  vim.system({ "mox", verb, target }, { text = true }, function(out)
    vim.schedule(function()
      notify_result(verb, out)
      if on_done then on_done(out) end
    end)
  end)
end

local function show_scratch(name, lines, ft)
  vim.cmd("botright new")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].modifiable = false
  if ft then vim.bo[buf].filetype = ft end
  vim.api.nvim_buf_set_name(buf, name)
end

function M.setup()
  local grp = vim.api.nvim_create_augroup("Lib.mox", { clear = true })
  local src_root = M.defaults().src_root

  vim.api.nvim_create_autocmd({ "BufNewFile", "BufReadPost" }, {
    group = grp,
    pattern = src_root .. "/*",
    callback = function(ev)
      vim.b[ev.buf].mox_source = true
    end,
  })

  vim.api.nvim_create_autocmd("BufWritePost", {
    group = grp,
    pattern = src_root .. "/*",
    callback = function(ev)
      if vim.g.mox_apply_on_save == false or vim.b[ev.buf].mox_apply_on_save == false then return end
      local live = M.live_of(vim.api.nvim_buf_get_name(ev.buf))
      if live then run_scoped("apply", live) end
    end,
  })

  vim.api.nvim_create_user_command("MoxSource", function()
    local src = M.source_of(buf_path())
    if not src or not vim.uv.fs_stat(src) then
      return vim.notify("mox: no source for this file", vim.log.levels.WARN)
    end
    vim.cmd.edit(vim.fn.fnameescape(src))
  end, { desc = "Edit the mox source behind this live file" })

  vim.api.nvim_create_user_command("MoxLive", function()
    local live = M.live_of(buf_path())
    if not live then
      return vim.notify("mox: not a mox source file", vim.log.levels.WARN)
    end
    vim.cmd.edit(vim.fn.fnameescape(live))
  end, { desc = "Edit the live file this mox source composes" })

  vim.api.nvim_create_user_command("MoxApply", function()
    local live = live_target_of_current()
    if not live then
      return vim.notify("mox: this file is not managed", vim.log.levels.WARN)
    end
    run_scoped("apply", live, function(out)
      if out.code == 0 then vim.notify("mox: applied " .. vim.fn.fnamemodify(live, ":~")) end
    end)
  end, { desc = "mox apply, scoped to this file" })

  vim.api.nvim_create_user_command("MoxDiff", function()
    local live = live_target_of_current()
    if not live then
      return vim.notify("mox: this file is not managed", vim.log.levels.WARN)
    end
    vim.system({ "mox", "diff", live }, { text = true }, function(out)
      vim.schedule(function()
        local text = (out.stdout or "")
        if text == "" then text = "(no difference)" end
        show_scratch("mox://diff", vim.split(text, "\n"), "diff")
      end)
    end)
  end, { desc = "mox diff, scoped to this file" })

  vim.api.nvim_create_user_command("MoxStatus", function()
    vim.system({ "mox", "status" }, { text = true }, function(out)
      vim.schedule(function()
        show_scratch("mox://status", vim.split(out.stdout or "", "\n"))
      end)
    end)
  end, { desc = "mox status for the whole tree" })
end

return M
