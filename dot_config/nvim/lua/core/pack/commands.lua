local M = {}

local function notify(msg, level)
  level = level or vim.log.levels.INFO
  local hl = (level >= vim.log.levels.ERROR and "ErrorMsg")
    or (level >= vim.log.levels.WARN and "WarningMsg")
    or "Normal"
  pcall(vim.api.nvim_echo, { { tostring(msg), hl } }, true, {})
  vim.notify(msg, level)
end

function M.setup(Pack)
  vim.api.nvim_create_user_command("PackStatus", function()
    local UI = require("core.pack.ui")
    local function render(filter_pattern)
      local data = Pack._structured_status(filter_pattern)
      local view
      view = UI.status(data.lines, {
        title = "core.pack: status",
        highlights = data.highlights,
        on_filter = function(p)
          view:close()
          render(p)
        end,
      })
    end
    render("")
  end, { desc = "List registered plugin specs (f to filter, F to clear)" })

  vim.api.nvim_create_user_command("PackInstall", function()
    local Install = require("core.pack.install")
    local specs = {}
    for _, s in pairs(Pack._specs) do specs[#specs + 1] = s end
    Install.install_missing(specs, {
      open_window = true,
      on_complete = function()
        vim.notify("core.pack: install complete")
      end,
    })
  end, { desc = "Install any plugins missing on disk" })

  vim.api.nvim_create_user_command("PackUpdate", function(opts)
    local Install = require("core.pack.install")
    local specs = {}
    for _, s in pairs(Pack._specs) do specs[#specs + 1] = s end
    local target = opts.bang and "lockfile" or "remote"
    Install.update(specs, opts.fargs, { confirm = true, target = target })
  end, {
    bang = true,
    nargs = "*",
    complete = function(arglead)
      local out = {}
      for name in pairs(Pack._specs) do
        if name:lower():find(arglead:lower(), 1, true) then out[#out + 1] = name end
      end
      table.sort(out)
      return out
    end,
    desc = "Update plugin(s); ! = target lockfile revs (use after :PackRollback)",
  })

  vim.api.nvim_create_user_command("PackClean", function()
    local Install = require("core.pack.install")
    local UI = require("core.pack.ui")
    local specs = {}
    for _, s in pairs(Pack._specs) do specs[#specs + 1] = s end
    Install.clean(specs, {
      on_review = function(orphans, do_remove)
        UI.clean_review(orphans, {
          on_apply = function(list) do_remove(list) end,
        })
      end,
    })
  end, { desc = "Remove plugins not in spec (with confirmation buffer)" })

  vim.api.nvim_create_user_command("PackSync", function()
    vim.cmd("PackUpdate")
    vim.cmd("PackClean")
  end, { desc = "Update then clean" })

  vim.api.nvim_create_user_command("PackRollback", function(opts)
    local History = require("core.pack.history")
    local UI = require("core.pack.ui")
    local entries = History.list()
    if #entries == 0 then notify("core.pack: no snapshots", vim.log.levels.WARN); return end

    -- Enrich with plugin counts (cheap: read each snapshot JSON).
    for _, e in ipairs(entries) do
      local fd = io.open(e.path, "r")
      if fd then
        local raw = fd:read("*a"); fd:close()
        local ok, data = pcall(vim.json.decode, raw)
        e.plugin_count = (ok and type(data) == "table" and type(data.plugins) == "table") and vim.tbl_count(data.plugins) or 0
      else
        e.plugin_count = 0
      end
    end

    local function apply(snapshot)
      local data = History.restore(snapshot.ts)
      if not data then notify("core.pack: restore failed", vim.log.levels.ERROR); return end
      notify(("core.pack: restored snapshot %s — run :PackUpdate! to apply"):format(snapshot.iso))
    end

    -- Numeric arg = direct index (1 = newest).
    local idx = tonumber(opts.fargs[1])
    if idx then
      local e = entries[idx]
      if not e then notify(("core.pack: no snapshot at index %d"):format(idx), vim.log.levels.WARN); return end
      apply(e)
      return
    end

    UI.rollback_review(entries, {
      on_select = function(snapshot) apply(snapshot) end,
    })
  end, {
    nargs = "?",
    desc = "Rollback lockfile to a previous snapshot (use :PackUpdate! after to apply)",
  })

  vim.api.nvim_create_user_command("PackLog", function(opts)
    local Log = require("core.pack.log")
    local UI = require("core.pack.ui")
    local limit = tonumber(opts.fargs[1]) or 50
    local entries = Log.list({ limit = limit })
    if #entries == 0 then notify("core.pack: no log entries"); return end

    local lines = { ("core.pack log — last %d entries"):format(#entries), "" }
    local highlights = { { 0, 0, #lines[1], "Title" } }

    -- Compute name column width: longest actual, floor 16, cap 40.
    local name_max = 16
    for _, e in ipairs(entries) do
      local n = e.name or ""
      if #n > name_max then name_max = #n end
    end
    if name_max > 40 then name_max = 40 end

    local now = os.time()
    for _, e in ipairs(entries) do
      local age_s = now - (e.ts or now)
      local ago
      if age_s < 60        then ago = ("%ds ago"):format(age_s)
      elseif age_s < 3600  then ago = ("%dm ago"):format(math.floor(age_s / 60))
      elseif age_s < 86400 then ago = ("%dh ago"):format(math.floor(age_s / 3600))
      else                       ago = ("%dd ago"):format(math.floor(age_s / 86400))
      end

      local ago_padded = ("%-9s"):format(ago)
      local raw_name = e.name or ""
      if #raw_name > name_max then raw_name = raw_name:sub(1, name_max - 1) .. "…" end
      local name_padded = ("%-" .. name_max .. "s"):format(raw_name)
      local range = e.from and e.to and ("%s..%s"):format(e.from:sub(1, 7), e.to:sub(1, 7)) or "(initial install)"
      local count_str = e.count and ("%d commits"):format(e.count) or ""
      local subject = e.subject or ""
      if #subject > 60 then subject = subject:sub(1, 59) .. "…" end

      local line = ("  %s  %s  %s  %s  %s"):format(ago_padded, name_padded, range, count_str, subject)
      local row = #lines

      local col = 2
      table.insert(highlights, { row, col, col + #ago_padded, "DiagnosticHint" }); col = col + #ago_padded + 2
      table.insert(highlights, { row, col, col + #name_padded, "Identifier" }); col = col + #name_padded + 2
      table.insert(highlights, { row, col, col + #range, "Comment" }); col = col + #range + 2
      table.insert(highlights, { row, col, col + #count_str, "Number" }); col = col + #count_str + 2
      table.insert(highlights, { row, col, col + #subject, "Comment" })

      lines[#lines + 1] = line
    end

    UI.status(lines, {
      title = "core.pack: log",
      highlights = highlights,
      filetype = "PackLog",
    })
  end, {
    nargs = "?",
    desc = "Show recent pack update log entries",
  })

  vim.api.nvim_create_user_command("PackBuild", function(opts)
    local Install = require("core.pack.install")
    local UI = require("core.pack.ui")
    local target = opts.fargs[1]
    local fidget = UI.fidget({ open_window = true })
    local total = 0
    for _, s in pairs(Pack._specs) do
      if s.build and (target == nil or s.name == target) then total = total + 1 end
    end
    if total == 0 then
      notify(target and ("core.pack: no build hook for " .. target) or "core.pack: no plugins with build hooks")
      fidget:close()
      return
    end
    fidget:set_status("core.pack", ("rebuilding 0/%d"):format(total))
    local done = 0
    for _, s in pairs(Pack._specs) do
      if s.build and (target == nil or s.name == target) then
        Install.run_build(s, Install.install_dir(s.name), { fidget = fidget })
        done = done + 1
        fidget:set_status("core.pack", ("rebuilding %d/%d"):format(done, total))
      end
    end
    fidget:done("core.pack")
  end, {
    nargs = "?",
    complete = function(arglead)
      local out = {}
      for name, s in pairs(Pack._specs) do
        if s.build and name:lower():find(arglead:lower(), 1, true) then out[#out + 1] = name end
      end
      table.sort(out)
      return out
    end,
    desc = "Re-run plugin build hooks (one or all)",
  })

  vim.api.nvim_create_user_command("PackLoad", function(opts)
    local name = opts.fargs[1]
    if not name then notify("core.pack: usage :PackLoad <name>", vim.log.levels.WARN); return end
    if not Pack.has(name) then notify(("core.pack: unknown plugin '%s'"):format(name), vim.log.levels.WARN); return end
    if Pack.loaded(name) then notify(("core.pack: %s already loaded"):format(name)); return end
    Pack.load(name)
    notify(("core.pack: loaded %s"):format(name))
  end, {
    nargs = 1,
    complete = function(arglead)
      local out = {}
      for n in pairs(Pack._specs) do
        if not Pack._loaded[n] and n:lower():find(arglead:lower(), 1, true) then out[#out + 1] = n end
      end
      table.sort(out)
      return out
    end,
    desc = "Manually load a lazy plugin by name",
  })
end

return M
