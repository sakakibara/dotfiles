local M = {}

local function name_complete(Pack, arglead, predicate)
  local out = {}
  for n, s in pairs(Pack._specs) do
    if (predicate == nil or predicate(n, s)) and n:lower():find(arglead:lower(), 1, true) then
      out[#out + 1] = n
    end
  end
  table.sort(out)
  return out
end

-- Build the status buffer payload (lines + highlight ranges) for :Pack
-- status. Pure function: reads Pack state, returns rendering data — no
-- side effects. Trigger column has a fixed 180-char width budget since
-- :Pack status renders before the scratch window is open and can't see
-- the real window width yet.
local function structured_status(Pack, filter_pattern)
  local Profile = require("core.profile")
  local names = vim.tbl_keys(Pack._specs)
  table.sort(names)
  if filter_pattern and filter_pattern ~= "" then
    local fp = filter_pattern:lower()
    local filtered = {}
    for _, n in ipairs(names) do
      if n:lower():find(fp, 1, true) then filtered[#filtered + 1] = n end
    end
    names = filtered
  end

  local loaded_count = 0
  local lazy_count = 0
  for _, n in ipairs(names) do
    if Pack._loaded[n] then loaded_count = loaded_count + 1
    elseif Pack._specs[n].lazy then lazy_count = lazy_count + 1 end
  end

  -- Name column: longest actual name, floor 16, cap 50.
  local name_max = 16
  for _, n in ipairs(names) do name_max = math.max(name_max, #n) end
  if name_max > 50 then name_max = 50 end

  local LOAD_W = 9

  -- Trigger column gets remainder. Fixed prefix bytes:
  --   2 (lead) + 4 (padded glyph) + 2 + name_max + 2 + 8 (state) + 2 + 9 (load) + 2 = 29 + name_max
  local trigger_max = 180 - (28 + name_max)
  if trigger_max < 30 then trigger_max = 30 end

  local total_load_ms = 0
  for _, n in ipairs(names) do
    local lt = Profile.lookup(n)
    if lt then total_load_ms = total_load_ms + lt end
  end
  local lines = {
    ("core.pack: %d registered (%d lazy, %d loaded, %.0f ms total load)"):format(#names, lazy_count, loaded_count, total_load_ms),
    "",
  }
  local highlights = { { 0, 0, #lines[1], "Title" } }

  for _, n in ipairs(names) do
    local s = Pack._specs[n]
    local state, glyph, glyph_hl
    if Pack._loaded[n] then
      state, glyph, glyph_hl = "loaded", "*", "Special"
    elseif s.lazy then
      state, glyph, glyph_hl = "lazy", "~", "Identifier"
    else
      state, glyph, glyph_hl = "pending", ".", "Comment"
    end

    local trigger = s.event and ("event=" .. vim.inspect(s.event):gsub("\n%s*", ""))
        or s.ft and ("ft=" .. vim.inspect(s.ft))
        or s.cmd and ("cmd=" .. vim.inspect(s.cmd))
        or s.keys and "keys"
        or ("priority=" .. s.priority)

    local name_truncated = n
    if #name_truncated > name_max then name_truncated = name_truncated:sub(1, name_max - 1) .. "…" end
    local name_padded = ("%-" .. name_max .. "s"):format(name_truncated)
    local state_padded = ("%-8s"):format(state)
    local load_ms = Profile.lookup(n)
    local load_str = load_ms and ("%6.2f ms"):format(load_ms) or "       - "
    local load_padded = ("%-" .. LOAD_W .. "s"):format(load_str)
    local trigger_truncated = trigger
    if #trigger_truncated > trigger_max then trigger_truncated = trigger_truncated:sub(1, trigger_max - 1) .. "…" end
    local line = ("  %s  %s  %s  %s  %s"):format(glyph, name_padded, state_padded, load_padded, trigger_truncated)
    local row = #lines

    -- col offsets (glyph is always 1 cell wide)
    local col = 2
    table.insert(highlights, { row, col, col + #glyph, glyph_hl }); col = col + #glyph + 2
    table.insert(highlights, { row, col, col + #name_padded, "Identifier" }); col = col + #name_padded + 2
    table.insert(highlights, { row, col, col + #state_padded, "Type" }); col = col + #state_padded + 2
    table.insert(highlights, { row, col, col + #load_padded, "Number" }); col = col + #load_padded + 2
    table.insert(highlights, { row, col, col + #trigger_truncated, "Comment" })

    lines[#lines + 1] = line
  end

  return { lines = lines, highlights = highlights }
end

local function subcommands(Pack)
  local subs = {}

  subs.status = {
    desc = "list registered plugin specs (f to filter, F to clear)",
    run = function()
      local UI = require("core.pack.ui")
      local function render(filter_pattern)
        local data = structured_status(Pack, filter_pattern)
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
    end,
  }

  subs.install = {
    desc = "install any plugins missing on disk",
    run = function()
      local Install = require("core.pack.install")
      local specs = {}
      for _, s in pairs(Pack._specs) do specs[#specs + 1] = s end
      Install.install_missing(specs, {
        open_window = true,
        on_complete = function() vim.notify("core.pack: install complete") end,
      })
    end,
  }

  subs.update = {
    desc = "update plugin(s); ! = target lockfile revs (use after :Pack rollback)",
    complete = function(arglead) return name_complete(Pack, arglead) end,
    run = function(opts)
      local Install = require("core.pack.install")
      local specs = {}
      for _, s in pairs(Pack._specs) do specs[#specs + 1] = s end
      local target = opts.bang and "lockfile" or "remote"
      Install.update(specs, opts.fargs, { confirm = true, target = target })
    end,
  }

  subs.clean = {
    desc = "remove plugins not in spec (with confirmation buffer)",
    run = function()
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
    end,
  }

  subs.sync = {
    desc = "update then clean",
    run = function()
      vim.cmd("Pack update")
      vim.cmd("Pack clean")
    end,
  }

  subs.rollback = {
    desc = "rollback lockfile to a previous snapshot (run :Pack update! to apply)",
    run = function(opts)
      local History = require("core.pack.history")
      local UI = require("core.pack.ui")
      local entries = History.list()
      if #entries == 0 then vim.notify("core.pack: no snapshots", vim.log.levels.WARN); return end

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
        if not data then vim.notify("core.pack: restore failed", vim.log.levels.ERROR); return end
        vim.notify(("core.pack: restored snapshot %s — run :Pack update! to apply"):format(snapshot.iso))
      end

      -- Numeric arg = direct index (1 = newest).
      local idx = tonumber(opts.fargs[1])
      if idx then
        local e = entries[idx]
        if not e then vim.notify(("core.pack: no snapshot at index %d"):format(idx), vim.log.levels.WARN); return end
        apply(e)
        return
      end

      UI.rollback_review(entries, {
        on_select = function(snapshot) apply(snapshot) end,
      })
    end,
  }

  subs.log = {
    desc = "show recent pack update log entries",
    run = function(opts)
      local Log = require("core.pack.log")
      local UI = require("core.pack.ui")
      local limit = tonumber(opts.fargs[1]) or 50
      local entries = Log.list({ limit = limit })
      if #entries == 0 then vim.notify("core.pack: no log entries"); return end

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
    end,
  }

  subs.build = {
    desc = "re-run plugin build hooks (one or all)",
    complete = function(arglead)
      return name_complete(Pack, arglead, function(_n, s) return s.build ~= nil end)
    end,
    run = function(opts)
      local Install = require("core.pack.install")
      local UI = require("core.pack.ui")
      local target = opts.fargs[1]
      local fidget = UI.fidget({ open_window = true })
      local total = 0
      for _, s in pairs(Pack._specs) do
        if s.build and (target == nil or s.name == target) then total = total + 1 end
      end
      if total == 0 then
        vim.notify(target and ("core.pack: no build hook for " .. target) or "core.pack: no plugins with build hooks")
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
    end,
  }

  subs.load = {
    desc = "manually load a lazy plugin by name",
    complete = function(arglead)
      return name_complete(Pack, arglead, function(n) return not Pack._loaded[n] end)
    end,
    run = function(opts)
      local name = opts.fargs[1]
      if not name then vim.notify("core.pack: usage :Pack load <name>", vim.log.levels.WARN); return end
      if not Pack.has(name) then vim.notify(("core.pack: unknown plugin '%s'"):format(name), vim.log.levels.WARN); return end
      if Pack.loaded(name) then vim.notify(("core.pack: %s already loaded"):format(name)); return end
      Pack.load(name)
      vim.notify(("core.pack: loaded %s"):format(name))
    end,
  }

  subs.profile = {
    desc = "show pack startup profile in a scratch buffer",
    run = function()
      local Profile = require("core.profile")
      local UI = require("core.pack.ui")
      local data = Profile._structured_report()
      UI.status(data.lines, {
        title = "core.pack: profile",
        highlights = data.highlights,
        filetype = "PackProfile",
      })
    end,
  }

  return subs
end

function M.setup(Pack)
  local subs = subcommands(Pack)

  vim.api.nvim_create_user_command("Pack", function(opts)
    local fargs = opts.fargs
    local sub_name = fargs[1] or "status"
    local handler = subs[sub_name]
    if not handler then
      local names = vim.tbl_keys(subs); table.sort(names)
      vim.notify(("core.pack: unknown subcommand '%s' (available: %s)")
        :format(sub_name, table.concat(names, ", ")), vim.log.levels.WARN)
      return
    end
    handler.run({
      fargs = vim.list_slice(fargs, 2),
      bang = opts.bang,
    })
  end, {
    bang = true,
    nargs = "*",
    complete = function(arglead, cmdline, _cursorpos)
      -- Strip the leading "Pack" / "Pack!" so tokens[1] = subcommand name (or
      -- the partial subcommand the user is typing), tokens[2..] = its args.
      local args_str = cmdline:gsub("^Pack!?%s*", "", 1)
      local tokens = vim.split(args_str, "%s+", { trimempty = false })
      if #tokens <= 1 then
        local out = {}
        for name in pairs(subs) do
          if name:lower():find(arglead:lower(), 1, true) then out[#out + 1] = name end
        end
        table.sort(out)
        return out
      end
      local handler = subs[tokens[1]]
      if handler and handler.complete then return handler.complete(arglead) end
      return {}
    end,
    desc = "core.pack — :Pack {install|update|status|log|build|load|rollback|clean|sync|profile}",
  })
end

return M
