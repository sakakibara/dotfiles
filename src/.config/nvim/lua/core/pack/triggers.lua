-- Lazy-load trigger registration and event re-firing.
--
-- Grouping at registration time (one autocmd per event, not per spec) prevents
-- fan-out at the source: N specs sharing BufWritePre produce 1 re-fire, not N.
--
-- A trigger loads its specs synchronously inside the dispatch that fired it,
-- then re-fires the event for the autocmds that load added, and only those:
-- handlers that already ran for this dispatch never run twice, and a newly
-- loaded BufWritePre handler still runs before the write. New autocmds fire
-- in definition order: a run of them that is a whole, all-new group fires as
-- that group; the rest fire through temporary copies. Copies of Vimscript
-- commands that use script-local names cannot run outside their script and
-- are skipped. When a trigger's load adds runtime dirs carrying ftplugin,
-- indent or syntax files for the triggering buffer's filetype whose loaders
-- already ran, those loaders run again for that buffer, followed once, in
-- definition order, by the FileType autocmds that ran after them, and, while
-- the buffer is still being created or read, by editorconfig and modelines.
-- Other buffers and loads by cmd, keys, VeryLazy or pack.load are left alone.

local M = {}

-- Build a trigger registrar bound to dependencies (load_spec,
-- install_spec_stubs) from core.pack.init. Returns the register function
-- that walks specs and installs autocmds.
function M.create(deps)
  local load_spec          = assert(deps.load_spec)
  local install_spec_stubs = assert(deps.install_spec_stubs)

  local copies = 0

  local function autocmd_key(a)
    if a.id then return a.id end
    return table.concat({
      tostring(a.group), a.pattern or "", tostring(a.buffer), a.command or "", tostring(a.once),
    }, "\0")
  end

  local function snapshot(event)
    local counts = {}
    for _, a in ipairs(vim.api.nvim_get_autocmds({ event = event })) do
      local k = autocmd_key(a)
      counts[k] = (counts[k] or 0) + 1
    end
    return counts
  end

  local function units_of(picked, has_old)
    local first, last = {}, {}
    for i, a in ipairs(picked) do
      if a.group then
        first[a.group] = first[a.group] or i
        last[a.group] = i
      end
    end
    local function contiguous(g)
      for j = first[g], last[g] do
        if picked[j].group ~= g then return false end
      end
      return true
    end
    local units, i = {}, 1
    while i <= #picked do
      local g = picked[i].group
      if g and not has_old[g] and first[g] == i and contiguous(g) then
        units[#units + 1] = { group = g }
        i = last[g] + 1
      else
        local u = units[#units]
        if not (u and u.copies) then
          u = { copies = {} }
          units[#units + 1] = u
        end
        u.copies[#u.copies + 1] = picked[i]
        i = i + 1
      end
    end
    return units
  end

  local function added_since(event, before)
    local list = vim.api.nvim_get_autocmds({ event = event })
    local seen, new, has_old = {}, {}, {}
    for _, a in ipairs(list) do
      local k = autocmd_key(a)
      seen[k] = (seen[k] or 0) + 1
      if seen[k] > (before[k] or 0) then
        new[#new + 1] = a
      elseif a.group then
        has_old[a.group] = true
      end
    end
    return units_of(new, has_old), list
  end

  local function same_slot(a, b)
    return a.group == b.group and a.buflocal == b.buflocal
      and (a.buflocal and a.buffer == b.buffer or (not a.buflocal and a.pattern == b.pattern))
  end

  local function copy_autocmd(event, a, group, list)
    local opts = { group = group, desc = a.desc, once = a.once }
    if a.buflocal then opts.buffer = a.buffer else opts.pattern = a.pattern end
    if a.callback == nil then
      if a.command:lower():find("<sid>", 1, true) or a.command:find("%f[%w_:]s:[%w_]") then return end
      if a.once then
        local n = 0
        for _, b in ipairs(list) do
          if same_slot(a, b) then n = n + 1 end
        end
        if n ~= 1 then return end
        local slot = { event = event, group = a.group }
        if a.buflocal then slot.buf = a.buffer else slot.pattern = a.pattern end
        local command = a.command
        opts.callback = function()
          pcall(vim.api.nvim_clear_autocmds, slot)
          vim.cmd(command)
        end
      else
        opts.command = a.command
      end
    else
      local cb = a.callback
      opts.callback = function(ev)
        local r
        if type(cb) == "function" then r = cb(ev) else r = vim.fn.call(cb, { ev }) end
        if a.id and (a.once or r == true) then pcall(vim.api.nvim_del_autocmd, a.id) end
        return r
      end
    end
    vim.api.nvim_create_autocmd(event, opts)
  end

  local function in_buf(buf, fn)
    if buf and vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_call(buf, fn)
    else
      fn()
    end
  end

  local function exec_event(event, opts)
    local ok, err = pcall(vim.api.nvim_exec_autocmds, event, opts)
    if not ok then
      vim.notify(("core.pack refire(%s): %s"):format(event, err), vim.log.levels.ERROR)
    end
  end

  local function run_units(event, units, list, base)
    for _, u in ipairs(units) do
      if u.group then
        exec_event(event, vim.tbl_extend("force", base, { group = u.group }))
      else
        copies = copies + 1
        local tmp = vim.api.nvim_create_augroup("core.pack.refire." .. copies, { clear = true })
        for _, a in ipairs(u.copies) do copy_autocmd(event, a, tmp, list) end
        exec_event(event, vim.tbl_extend("force", base, { group = tmp }))
        pcall(vim.api.nvim_del_augroup_by_id, tmp)
      end
    end
  end

  local function fire_added(event, args, before)
    local units, list = added_since(event, before)
    if #units == 0 then return end
    local base = {
      pattern = (args.match and args.match ~= "") and args.match or nil,
      data = args.data,
      modeline = false,
    }
    in_buf(args.buf, function() run_units(event, units, list, base) end)
  end

  local function rtp_set()
    local set = {}
    for _, d in ipairs(vim.api.nvim_list_runtime_paths()) do set[d] = true end
    return set
  end

  local function added_dirs(before)
    local out = {}
    for _, d in ipairs(vim.api.nvim_list_runtime_paths()) do
      if not before[d] then out[#out + 1] = d end
    end
    return out
  end

  local KINDS = {
    ftplugin = { "%s.vim", "%s.lua", "%s_*.vim", "%s_*.lua", "%s/*.vim", "%s/*.lua" },
    indent   = { "%s.vim", "%s.lua" },
    syntax   = { "%s.vim", "%s.lua", "%s/*.vim", "%s/*.lua" },
  }

  local function carries(dirs, kind, value)
    local path = table.concat(vim.tbl_map(function(d) return (d:gsub(",", "\\,")) end, dirs), ",")
    for _, name in ipairs(vim.split(value, ".", { plain = true, trimempty = true })) do
      for _, pat in ipairs(KINDS[kind]) do
        if #vim.fn.globpath(path, kind .. "/" .. pat:format(name), false, true) > 0 then return true end
      end
    end
    return false
  end

  local LOADERS = {
    { group = "filetypeplugin", kind = "ftplugin" },
    { group = "filetypeindent", kind = "indent" },
    { group = "syntaxset", kind = "syntax" },
  }
  local LOADER_GROUPS = {}
  for _, l in ipairs(LOADERS) do LOADER_GROUPS[l.group] = true end

  local function matches(a, buf, ft)
    if a.buflocal then return a.buffer == buf end
    local ok, re = pcall(vim.regex, vim.fn.glob2regpat(a.pattern or "*"))
    return ok and re:match_str(ft) ~= nil
  end

  local function refresh_buffer(buf, dirs, before, loader_id)
    local ft = vim.bo[buf].filetype
    if ft == "" then return end
    local list = vim.api.nvim_get_autocmds({ event = "FileType" })
    local stop = #list
    if loader_id then
      for i, a in ipairs(list) do
        if a.id == loader_id then stop = i - 1; break end
      end
    end
    local rerun, start = {}, nil
    for _, l in ipairs(LOADERS) do
      for i = 1, stop do
        if list[i].group_name == l.group then
          if carries(dirs, l.kind, ft) then
            rerun[l.group] = true
            start = math.min(start or i, i)
          end
          break
        end
      end
    end
    if not start then return end
    local seen, picked, has_old = {}, {}, {}
    for i, a in ipairs(list) do
      local k = autocmd_key(a)
      seen[k] = (seen[k] or 0) + 1
      if matches(a, buf, ft) then
        local g = a.group_name
        local ran = i >= start and i <= stop and seen[k] <= (before[k] or 0) and not a.once
          and g ~= "core.pack.triggers" and (rerun[g] or not LOADER_GROUPS[g])
        if ran then
          picked[#picked + 1] = a
        elseif a.group then
          has_old[a.group] = true
        end
      end
    end
    run_units("FileType", units_of(picked, has_old), list, { pattern = ft, modeline = false })
    return true
  end

  local function group_exists(name)
    return (pcall(vim.api.nvim_get_autocmds, { group = name }))
  end

  local function reapply(buf)
    local ec = vim.F.if_nil(vim.b[buf].editorconfig, vim.g.editorconfig, true)
    if ec ~= false and group_exists("nvim.editorconfig") then
      local ok, err = pcall(require("editorconfig").config, buf)
      if not ok then vim.notify("core.pack editorconfig: " .. tostring(err), vim.log.levels.ERROR) end
    end
    local g = vim.api.nvim_create_augroup("core.pack.modelines", { clear = true })
    vim.api.nvim_create_autocmd("User", { group = g, pattern = "CorePackModelines", callback = function() end })
    exec_event("User", { group = g, pattern = "CorePackModelines", modeline = true })
    pcall(vim.api.nvim_del_augroup_by_id, g)
  end

  local refreshing = false
  local queued = {}
  local refreshed = {}
  local fresh = {}

  local function run_job(job)
    local buf = job.buf
    if not (vim.api.nvim_buf_is_valid(buf) and vim.api.nvim_buf_is_loaded(buf)) then return end
    local got = refreshed[buf] or {}
    refreshed[buf] = got
    local dirs = {}
    for _, d in ipairs(job.dirs) do
      if not got[d] then
        got[d] = true
        dirs[#dirs + 1] = d
      end
    end
    if #dirs == 0 then return end
    in_buf(buf, function()
      if refresh_buffer(buf, dirs, job.before, job.loader_id) and fresh[buf] then reapply(buf) end
    end)
  end

  local function refresh_trigger(job)
    if refreshing then
      queued[#queued + 1] = job
      return
    end
    refreshing = true
    local first_err
    while job do
      local ok, err = pcall(run_job, job)
      if not ok and not first_err then first_err = err end
      job = table.remove(queued, 1)
    end
    refreshing = false
    if first_err then
      vim.notify("core.pack refresh: " .. tostring(first_err), vim.log.levels.ERROR)
    end
  end

  local function load_refreshing(specs, reason, args, loader_id)
    local ft_before = snapshot("FileType")
    local rtp_before = rtp_set()
    for _, s in ipairs(specs) do load_spec(s, reason) end
    local dirs = added_dirs(rtp_before)
    local buf = args.buf
    if #dirs == 0 or not (buf and vim.api.nvim_buf_is_valid(buf)) then return end
    refresh_trigger({
      buf = buf, dirs = dirs, before = ft_before, loader_id = args.event == "FileType" and loader_id or nil,
    })
  end

  local function register(specs)
    local event = require("core.event")

    vim.api.nvim_create_autocmd({ "BufNew", "BufReadPre" }, {
      group = vim.api.nvim_create_augroup("core.pack.fresh", { clear = true }),
      callback = function(ev)
        local buf = ev.buf
        if fresh[buf] then return end
        fresh[buf] = true
        vim.schedule(function() fresh[buf] = nil end)
      end,
    })

    -- Group specs by triggering event/pattern/ft. Matches lazy.nvim's design:
    -- one once=true autocmd per trigger group loads all specs registered to
    -- it, so N specs sharing BufWritePre fan into 1 handler, not N. Prevents
    -- the E218 nesting class of bugs at the structural level -- no runtime
    -- dedup needed for the common case.
    local user_groups  = {}  -- [pattern] -> { specs = {...} }
    local event_groups = {}  -- [event]   -> { specs = {...} }
    local ft_groups    = {}  -- [ft]      -> { specs = {...} }

    local function push(map, key, spec)
      local g = map[key]
      if not g then
        g = { specs = {} }
        map[key] = g
      end
      table.insert(g.specs, spec)
    end

    for _, spec in ipairs(specs) do
      if spec.lazy and spec.event then
        for _, e in ipairs(event.expand(spec.event)) do
          if e == "VeryLazy" then
            event.on("VeryLazy", function() load_spec(spec, "event:VeryLazy") end)
          elseif e:match("^User ") then
            push(user_groups, e:sub(6), spec)
          else
            push(event_groups, e, spec)
          end
        end
      end

      if spec.lazy and spec.ft then
        local fts = type(spec.ft) == "table" and spec.ft or { spec.ft }
        for _, ft in ipairs(fts) do push(ft_groups, ft, spec) end
      end

      -- cmd (lazy only) -- registered per-command, not per-spec, so no
      -- grouping concern; two specs binding the same command is already a
      -- user error caught at vim.api.nvim_create_user_command.
      if spec.lazy and spec.cmd then
        local cmds = type(spec.cmd) == "table" and spec.cmd or { spec.cmd }
        for _, c in ipairs(cmds) do
          vim.api.nvim_create_user_command(c, function(opts)
            pcall(vim.api.nvim_del_user_command, c)
            load_spec(spec, "cmd:" .. c)
            local range = ""
            if opts.range == 2 then
              range = opts.line1 .. "," .. opts.line2
            elseif opts.range == 1 then
              range = tostring(opts.line1)
            end
            vim.cmd(("%s %s%s%s %s"):format(
              opts.mods, range, c, opts.bang and "!" or "", opts.args))
          end, { bang = true, nargs = "*", range = true, desc = "Lazy: " .. spec.name })
        end
      end

      -- keys: lazy plugins get stubs here. Eager plugins already had their
      -- real mappings installed in load_spec -> install_spec_keys during the
      -- eager-load pass; skipping avoids a spurious conflict warning and
      -- overhead.
      if spec.lazy and spec.keys then
        install_spec_stubs(spec)
      end
    end

    local function group_desc(group)
      local names = {}
      for _, s in ipairs(group.specs) do names[#names + 1] = s.name end
      return "lazy: " .. table.concat(names, ", ")
    end

    local function add_loader(event, opts, group, reason)
      local id, done
      opts.group = vim.api.nvim_create_augroup("core.pack.triggers", { clear = false })
      opts.nested = true
      opts.desc = group_desc(group)
      opts.callback = function(args)
        if done then return true end
        done = true
        local before = snapshot(args.event)
        load_refreshing(group.specs, reason, args, id)
        fire_added(args.event, args, before)
        return true
      end
      id = vim.api.nvim_create_autocmd(event, opts)
    end

    for pat, group in pairs(user_groups) do
      add_loader("User", { pattern = pat }, group, "event:User " .. pat)
    end

    for e, group in pairs(event_groups) do
      add_loader(e, {}, group, "event:" .. e)
    end

    for ft, group in pairs(ft_groups) do
      add_loader("FileType", { pattern = ft }, group, "ft:" .. ft)
    end
  end

  return { register = register }
end

return M
