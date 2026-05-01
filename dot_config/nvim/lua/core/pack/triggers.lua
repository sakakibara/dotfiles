-- Lazy-load trigger registration and event re-firing.
--
-- The re-fire scheduler tracks three states per (event, buffer, pattern) key:
--
-- - pending: scheduled via vim.schedule, not yet run
-- - active:  currently running its nvim_exec_autocmds
-- - retry:   request made while active; latest opts replayed after active
--
-- Grouping at registration time (one autocmd per event, not per spec) prevents
-- fan-out at the source: N specs sharing BufWritePre produce 1 re-fire, not N.
-- The dedup/guard/retry below are defense-in-depth for edge cases:
--
-- - Dedup: two different trigger sites (e.g. cascaded BufReadPre→BufReadPost)
--   scheduling the same-key refire in one tick collapse to one.
-- - Re-entrancy guard: `vim.schedule` alone does NOT break autocmd nesting
--   when a handler pumps the event loop (BufWritePre → conform.format →
--   vim.wait(1000) — libuv runs pending schedule callbacks during that
--   wait). Guard short-circuits any same-key recursion.
-- - Retry slot: a schedule arriving while active must not be dropped —
--   park it and replay after active clears.
--
-- Keyed by (event, buffer, pattern): different buffers/patterns are
-- independent work and must not block each other.

local M = {}

local notify = require("core.pack.util").notify

-- Build a trigger registrar bound to dependencies (load_spec,
-- install_spec_stubs) from core.pack.init. Returns the schedule_refire
-- closure (so init.lua can expose it as M._schedule_refire for tests) and
-- the register function that walks specs and installs autocmds.
function M.create(deps)
  local load_spec          = assert(deps.load_spec)
  local install_spec_stubs = assert(deps.install_spec_stubs)

  local pending = {}  -- scheduled, not yet run
  local active  = {}  -- currently running
  local retry   = {}  -- request made while active: latest opts to replay

  local schedule_refire
  schedule_refire = function(event, opts)
    local key = event .. "\0" .. (opts.buffer or 0) .. "\0" .. (opts.pattern or "")
    if active[key] then
      -- Park the request — we'll replay once active clears. Latest-wins is
      -- correct for lazy-load: each call carries opts from one original
      -- event fire, and the re-fire just needs to give handlers a chance
      -- to catch up on the current state.
      retry[key] = { event = event, opts = opts }
      return
    end
    if pending[key] then return end
    pending[key] = true
    vim.schedule(function()
      pending[key] = nil
      active[key]  = true
      local ok, err = pcall(vim.api.nvim_exec_autocmds, event, opts)
      active[key]  = nil
      if not ok then
        notify(("core.pack refire(%s): %s"):format(event, err), vim.log.levels.ERROR)
      end
      local r = retry[key]
      if r then
        retry[key] = nil
        schedule_refire(r.event, r.opts)
      end
    end)
  end

  local function register(specs)
    local event = require("core.event")

    -- Group specs by triggering event/pattern/ft. Matches lazy.nvim's design:
    -- one once=true autocmd per trigger group loads all specs registered to
    -- it, so N specs sharing BufWritePre fan into 1 handler, not N. Prevents
    -- the E218 nesting class of bugs at the structural level — no runtime
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
            event.on("VeryLazy", function() load_spec(spec) end)
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

      -- cmd (lazy only) — registered per-command, not per-spec, so no
      -- grouping concern; two specs binding the same command is already a
      -- user error caught at vim.api.nvim_create_user_command.
      if spec.lazy and spec.cmd then
        local cmds = type(spec.cmd) == "table" and spec.cmd or { spec.cmd }
        for _, c in ipairs(cmds) do
          vim.api.nvim_create_user_command(c, function(opts)
            pcall(vim.api.nvim_del_user_command, c)
            load_spec(spec)
            vim.api.nvim_cmd({
              cmd = c,
              bang = opts.bang,
              args = opts.fargs,
              range = opts.range > 0 and { opts.line1, opts.line2 } or nil,
              count = opts.count >= 0 and opts.count or nil,
              mods = opts.smods,
            }, {})
          end, { bang = true, nargs = "*", range = true, desc = "lazy: " .. spec.name })
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

    local function load_group(group)
      for _, s in ipairs(group.specs) do load_spec(s) end
    end

    for pat, group in pairs(user_groups) do
      vim.api.nvim_create_autocmd("User", {
        once = true, pattern = pat, desc = group_desc(group),
        callback = function(args)
          load_group(group)
          schedule_refire("User", { pattern = pat, data = args.data, modeline = false })
        end,
      })
    end

    for e, group in pairs(event_groups) do
      vim.api.nvim_create_autocmd(e, {
        once = true, desc = group_desc(group),
        callback = function(args)
          load_group(group)
          schedule_refire(args.event, {
            buffer = args.buf, data = args.data, modeline = false,
          })
        end,
      })
    end

    for ft, group in pairs(ft_groups) do
      vim.api.nvim_create_autocmd("FileType", {
        once = true, pattern = ft, desc = group_desc(group),
        callback = function(args)
          load_group(group)
          schedule_refire("FileType", { buffer = args.buf, modeline = false })
        end,
      })
    end
  end

  return { schedule_refire = schedule_refire, register = register }
end

return M
