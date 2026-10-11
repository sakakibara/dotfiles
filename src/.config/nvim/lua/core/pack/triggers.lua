-- Lazy-load trigger registration and event re-firing.
--
-- Grouping at registration time (one autocmd per event, not per spec) prevents
-- fan-out at the source: N specs sharing BufWritePre produce 1 re-fire, not N.
--
-- A trigger loads its specs synchronously inside the dispatch that fired it,
-- then re-fires the event for the autocmds that load added, and only those:
-- handlers that already ran for this dispatch never run twice, and a newly
-- loaded BufWritePre handler still runs before the write. Groups whose
-- autocmds for the event are all new fire as groups; the remaining new
-- autocmds (ungrouped, or added to a group that already had some) fire
-- through temporary copies. Copies of Vimscript commands that use
-- script-local names cannot run outside their script and are skipped.

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

  local function added_since(event, before)
    local seen, groups, loose = {}, {}, {}
    for _, a in ipairs(vim.api.nvim_get_autocmds({ event = event })) do
      local k = autocmd_key(a)
      seen[k] = (seen[k] or 0) + 1
      local is_new = seen[k] > (before[k] or 0)
      if a.group then
        local g = groups[a.group] or { old = false, new = {} }
        groups[a.group] = g
        if is_new then g.new[#g.new + 1] = a else g.old = true end
      elseif is_new then
        loose[#loose + 1] = a
      end
    end
    local whole = {}
    for id, g in pairs(groups) do
      if g.old then
        vim.list_extend(loose, g.new)
      elseif #g.new > 0 then
        whole[#whole + 1] = id
      end
    end
    table.sort(whole)
    return whole, loose
  end

  local function copy_autocmd(event, a, group)
    local opts = { group = group, desc = a.desc, once = a.once }
    if a.buflocal then opts.buffer = a.buffer else opts.pattern = a.pattern end
    if a.callback == nil then
      if a.command:lower():find("<sid>", 1, true) or a.command:find("%f[%w_:]s:[%w_]") then return end
      opts.command = a.command
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

  local function fire_added(event, args, before)
    local whole, loose = added_since(event, before)
    if #whole == 0 and #loose == 0 then return end
    local base = {
      pattern = (args.match and args.match ~= "") and args.match or nil,
      data = args.data,
      modeline = false,
    }
    local function exec(group)
      local ok, err = pcall(vim.api.nvim_exec_autocmds, event,
        vim.tbl_extend("force", base, { group = group }))
      if not ok then
        vim.notify(("core.pack refire(%s): %s"):format(event, err), vim.log.levels.ERROR)
      end
    end
    local function run()
      for _, g in ipairs(whole) do exec(g) end
      if #loose > 0 then
        copies = copies + 1
        local tmp = vim.api.nvim_create_augroup("core.pack.refire." .. copies, { clear = true })
        for _, a in ipairs(loose) do copy_autocmd(event, a, tmp) end
        exec(tmp)
        pcall(vim.api.nvim_del_augroup_by_id, tmp)
      end
    end
    if args.buf and vim.api.nvim_buf_is_valid(args.buf) then
      vim.api.nvim_buf_call(args.buf, run)
    else
      run()
    end
  end

  local function register(specs)
    local event = require("core.event")

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

    local function loader(group, reason)
      return function(args)
        local before = snapshot(args.event)
        for _, s in ipairs(group.specs) do load_spec(s, reason) end
        fire_added(args.event, args, before)
      end
    end

    for pat, group in pairs(user_groups) do
      vim.api.nvim_create_autocmd("User", {
        once = true, nested = true, pattern = pat, desc = group_desc(group),
        callback = loader(group, "event:User " .. pat),
      })
    end

    for e, group in pairs(event_groups) do
      vim.api.nvim_create_autocmd(e, {
        once = true, nested = true, desc = group_desc(group),
        callback = loader(group, "event:" .. e),
      })
    end

    for ft, group in pairs(ft_groups) do
      vim.api.nvim_create_autocmd("FileType", {
        once = true, nested = true, pattern = ft, desc = group_desc(group),
        callback = loader(group, "ft:" .. ft),
      })
    end
  end

  return { register = register }
end

return M
