-- Lazy-load trigger registration and event re-firing.
--
-- Grouping at registration time (one autocmd per event, not per spec) prevents
-- fan-out at the source: N specs sharing BufWritePre produce 1 re-fire, not N.
--
-- Each (event, buffer, pattern, filetype) key re-fires at most once, and only
-- once no autocmd dispatch is in progress: `vim.schedule` alone does NOT
-- break autocmd nesting when a handler pumps the event loop (BufWritePre ->
-- conform.format -> vim.wait(1000) -- libuv runs pending schedule callbacks
-- during that wait), so a re-fire that finds itself inside a dispatch waits
-- for it to finish.
--
-- Different buffers/patterns are independent work and must not block each
-- other.

local M = {}

-- Build a trigger registrar bound to dependencies (load_spec,
-- install_spec_stubs) from core.pack.init. Returns the schedule_refire
-- closure (so init.lua can expose it as M._schedule_refire for tests) and
-- the register function that walks specs and installs autocmds.
function M.create(deps)
  local load_spec          = assert(deps.load_spec)
  local install_spec_stubs = assert(deps.install_spec_stubs)

  local requested = {}

  local function in_autocmd()
    return (pcall(vim.fn.expand, "<amatch>"))
  end

  local function schedule_refire(event, opts)
    local ft = event == "FileType" and opts.buffer and vim.api.nvim_buf_is_valid(opts.buffer)
      and vim.bo[opts.buffer].filetype or ""
    local key = table.concat({ event, opts.buffer or 0, opts.pattern or "", ft }, "\0")
    if requested[key] then return end
    requested[key] = true
    local function run()
      if in_autocmd() then
        vim.defer_fn(run, 10)
        return
      end
      if opts.buffer and not vim.api.nvim_buf_is_valid(opts.buffer) then return end
      local ok, err = pcall(vim.api.nvim_exec_autocmds, event, opts)
      if not ok then
        vim.notify(("core.pack refire(%s): %s"):format(event, err), vim.log.levels.ERROR)
      end
    end
    vim.schedule(run)
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

    local function load_group(group, reason)
      for _, s in ipairs(group.specs) do load_spec(s, reason) end
    end

    for pat, group in pairs(user_groups) do
      vim.api.nvim_create_autocmd("User", {
        once = true, pattern = pat, desc = group_desc(group),
        callback = function(args)
          vim.schedule(function()
            load_group(group, "event:User " .. pat)
            schedule_refire("User", { pattern = pat, data = args.data, modeline = false })
          end)
        end,
      })
    end

    for e, group in pairs(event_groups) do
      vim.api.nvim_create_autocmd(e, {
        once = true, desc = group_desc(group),
        callback = function(args)
          vim.schedule(function()
            load_group(group, "event:" .. args.event)
            schedule_refire(args.event, {
              buffer = args.buf, data = args.data, modeline = false,
            })
          end)
        end,
      })
    end

    for ft, group in pairs(ft_groups) do
      vim.api.nvim_create_autocmd("FileType", {
        once = true, pattern = ft, desc = group_desc(group),
        callback = function(args)
          vim.schedule(function()
            load_group(group, "ft:" .. ft)
            schedule_refire("FileType", { buffer = args.buf, modeline = false })
          end)
        end,
      })
    end
  end

  return { schedule_refire = schedule_refire, register = register }
end

return M
