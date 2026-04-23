-- lua/core/pack.lua
local M = {}

M._specs = {}       -- { [name] = normalized_spec }
M._loaded = {}      -- { [name] = true }
M._opts = {}        -- { [name] = merged_opts }
M._on_load = {}     -- { [name] = { fn, ... } }
M._key_registry = {} -- { [mode..":"..lhs..":"..(ft or "")] = spec.name }
M._warned_conflicts = {} -- { [sig..":"..nameA..":"..nameB (sorted)] = true }

-- Pre-noice queuing and :messages tee are handled by wrapper A in
-- config/init.lua — we just forward to vim.notify here.
local function notify(msg, level)
  vim.notify(msg, level)
end

local ALLOWED_FIELDS = {
  [1] = true, src = true, name = true, version = true, branch = true,
  dependencies = true, event = true, ft = true, cmd = true, keys = true,
  priority = true, enabled = true, cond = true, dev = true, lazy = true,
  init = true, opts = true, config = true, main = true, build = true,
}

local function normalize(spec)
  if type(spec) == "string" then spec = { spec } end
  if type(spec) ~= "table" then error("spec must be a table, got " .. type(spec)) end

  -- Expand GitHub shorthand
  local src, name = spec.src, spec.name
  if spec[1] and not src then
    local shorthand = spec[1]
    if shorthand:match("^https?://") then
      src = shorthand
    else
      src = "https://github.com/" .. shorthand
    end
    if not name then name = shorthand:match("([^/]+)$") end
  end
  if not src and not spec.dev then error("spec missing src: " .. vim.inspect(spec)) end
  if not name then error("spec missing name (could not infer): " .. vim.inspect(spec)) end

  -- Warn on unknown fields
  for k in pairs(spec) do
    if type(k) == "string" and not ALLOWED_FIELDS[k] then
      notify(("core.pack: unknown field '%s' on spec '%s'"):format(k, name), vim.log.levels.WARN)
    end
  end

  local has_trigger = spec.event or spec.ft or spec.cmd or spec.keys
  local lazy = spec.lazy
  if lazy == nil then lazy = has_trigger ~= nil end

  -- Convert glob-style version ("1.*", "^1.0", etc.) into vim.version.range().
  -- Exact refs (tags, branches, commits) pass through unchanged. If `branch`
  -- is provided and `version` isn't, promote branch into version (vim.pack
  -- uses the `version` field for branch/tag/commit refs).
  local version = spec.version
  if version == nil and type(spec.branch) == "string" then
    version = spec.branch
  end
  if type(version) == "string" and version:match("[%*x%^~]") then
    local stripped = version:gsub("[%*x%^~]", ""):gsub("%.%.+", "."):gsub("^%.", ""):gsub("%.$", "")
    version = vim.version.range(stripped)
  end

  return {
    src = src,
    name = name,
    version = version,
    branch = spec.branch,
    dependencies = spec.dependencies or {},
    event = spec.event,
    ft = spec.ft,
    cmd = spec.cmd,
    keys = spec.keys,
    priority = spec.priority or 50,
    enabled = spec.enabled ~= false,
    cond = spec.cond,
    dev = spec.dev == true,
    lazy = lazy,
    init = spec.init,
    opts = spec.opts or {},
    config = spec.config,
    main = spec.main,
    build = spec.build,
  }
end

-- Keymaps. Two phases: (1) load_spec installs the real mapping after
-- run_config — zero wrapper overhead on every subsequent press. (2) For
-- lazy plugins, _register_triggers installs a stub; on first press the
-- stub loads the plugin, install_spec_keys overrides it, then feedkeys
-- ("m" mode) replays the key so the real mapping fires.
--
-- String rhs defaults to remap=true (correct for <Plug>, <Cmd>, etc.);
-- pass remap=false on the key entry to override.
local function base_map_opts(k)
  return {
    desc    = k.desc,
    silent  = k.silent ~= false,
    nowait  = k.nowait,
    expr    = k.expr,
    buffer  = k.buffer,
  }
end

local function remap_default(k)
  if k.remap ~= nil then return k.remap end
  return type(k[2]) == "string"
end

local function conflict_key(mode, lhs, ft)
  return mode .. ":" .. lhs .. ":" .. (ft or "")
end

local function register_lhs(spec, mode, lhs, ft)
  local sig = conflict_key(mode, lhs, ft)
  local owner = M._key_registry[sig]
  if owner and owner ~= spec.name then
    -- Dedup by canonical (sorted) pair so stub→real transitions don't re-warn:
    -- the same pair of specs conflicting on the same lhs is one conflict,
    -- regardless of which spec registered first.
    local a, b = owner, spec.name
    if a > b then a, b = b, a end
    local warn_key = sig .. "|" .. a .. "|" .. b
    if not M._warned_conflicts[warn_key] then
      M._warned_conflicts[warn_key] = true
      notify(
        ("core.pack: keymap conflict '%s' (mode=%s, ft=%s): '%s' vs '%s'")
          :format(lhs, mode, tostring(ft or "*"), owner, spec.name),
        vim.log.levels.WARN)
    end
  end
  M._key_registry[sig] = spec.name
end

-- Install one real keymap for (spec, k, mode). Handles ft-scoped (FileType
-- autocmd + scan of already-loaded matching buffers) and global mappings.
local function set_real_keymap(spec, k, m)
  local lhs = k[1] or k.lhs
  local rhs = k[2]
  local opts = base_map_opts(k)
  opts.remap = remap_default(k)
  opts.desc  = opts.desc or ("key: " .. spec.name)

  register_lhs(spec, m, lhs, k.ft)

  -- rhs==nil means "plugin's own setup owns the keymap" — we only track it
  -- for conflict detection and skip installing anything.
  if rhs == nil then return end

  if k.ft then
    local fts = type(k.ft) == "table" and k.ft or { k.ft }
    local function install_on_buf(args)
      local bopts = vim.tbl_extend("force", opts, { buffer = args.buf })
      vim.keymap.set(m, lhs, rhs, bopts)
    end
    vim.api.nvim_create_autocmd("FileType", { pattern = fts, callback = install_on_buf })
    -- Cover the buffer that triggered the lazy load: its FileType already
    -- fired, so the autocmd above won't install anything on it. Scan.
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_loaded(buf) then
        local cur_ft = vim.bo[buf].filetype
        for _, want in ipairs(fts) do
          if cur_ft == want then install_on_buf({ buf = buf }); break end
        end
      end
    end
  else
    vim.keymap.set(m, lhs, rhs, opts)
  end
end

-- After load, warn if a string <Plug> rhs doesn't resolve to any mapping.
-- Catches typos and plugin-renamed plugs.
local function validate_plug(spec, k, m)
  local rhs = k[2]
  if type(rhs) ~= "string" or not rhs:match("^<Plug>") then return end
  if vim.fn.maparg(rhs, m) == "" then
    notify(
      ("core.pack: unresolved <Plug> rhs for '%s' -> '%s' (mode=%s, spec=%s)")
        :format(k[1] or k.lhs, rhs, m, spec.name),
      vim.log.levels.WARN)
  end
end

local function for_each_mode(k, fn)
  local mode = k.mode or "n"
  local modes = type(mode) == "table" and mode or { mode }
  for _, m in ipairs(modes) do fn(m) end
end

local function install_spec_keys(spec)
  if not spec.keys then return end
  for _, k in ipairs(spec.keys) do
    for_each_mode(k, function(m)
      set_real_keymap(spec, k, m)
      validate_plug(spec, k, m)
    end)
  end
end

local function run_config(spec)
  if M._loaded[spec.name] then return end
  M._loaded[spec.name] = true

  local profile = require("core.profile")
  local ok, err
  profile.span("config:" .. spec.name, "config", function()
    ok, err = xpcall(function()
      if spec.config then
        spec.config(spec, spec.opts)
      else
        local mod = spec.main or spec.name:gsub("%.nvim$", "")
        local ok2, plugin = pcall(require, mod)
        if ok2 and type(plugin) == "table" and type(plugin.setup) == "function" then
          plugin.setup(spec.opts)
        elseif not ok2 then
          -- Clear sentinel left by Lua's require when the module errors
          -- mid-load, or subsequent explicit requires raise "loop or
          -- previous error loading module". Deps are auto-required here
          -- before the parent is packadded, so a missing transitive rtp
          -- entry (e.g. nvim-dap-virtual-text requiring 'dap') trips this.
          package.loaded[mod] = nil
        end
      end
    end, debug.traceback)
  end)

  if not ok then
    notify(("core.pack config(%s): %s"):format(spec.name, err), vim.log.levels.ERROR)
  end

  for _, fn in ipairs(M._on_load[spec.name] or {}) do
    local okfn, errfn = xpcall(fn, debug.traceback)
    if not okfn then notify(("on_load(%s): %s"):format(spec.name, errfn), vim.log.levels.ERROR) end
  end
  M._on_load[spec.name] = nil
end

local function packadd(spec)
  if spec.dev then return end  -- dev plugins: assume on rtp already or skip for tests
  -- Invariant: spec.name matches the vim.pack install directory under pack/core/opt/<name>.
  require("core.profile").span("packadd:" .. spec.name, "packadd", function()
    local ok, err = pcall(vim.cmd, "packadd " .. spec.name)
    if not ok then
      notify(("core.pack packadd(%s) failed: %s"):format(spec.name, err), vim.log.levels.ERROR)
    end
  end)
end

local _loading = {}  -- cycle-detection guard: names currently mid-load

local function load_spec(spec)
  if M._loaded[spec.name] then return end
  if _loading[spec.name] then
    notify(("core.pack: dependency cycle detected at '%s'"):format(spec.name), vim.log.levels.ERROR)
    return
  end
  _loading[spec.name] = true
  for _, dep_name in ipairs(spec.dependencies) do
    local dep = M._specs[dep_name]
    if dep then
      load_spec(dep)
    else
      notify(("core.pack: unknown dependency '%s' for '%s'"):format(dep_name, spec.name), vim.log.levels.WARN)
    end
  end
  packadd(spec)
  run_config(spec)
  -- install_spec_keys runs AFTER run_config so that:
  --   1. any <Plug> mapping the plugin installs in setup() is present when
  --      validate_plug checks it
  --   2. the real keymap overrides any stub that triggered this load, so the
  --      subsequent feedkeys replay (in "m" mode) hits the real mapping.
  install_spec_keys(spec)
  _loading[spec.name] = nil
end

-- Stub keymap for a lazy plugin: on first press, load the plugin (which
-- installs the real mapping via install_spec_keys), then replay the key in
-- "m" (remap) mode so the real mapping fires. count/register preserved.
local function set_stub_keymap(spec, k, m)
  local lhs = k[1] or k.lhs
  local opts = base_map_opts(k)
  opts.remap = nil  -- stub is a plain noremap callback; it doesn't need remap
  opts.desc  = opts.desc or ("lazy: " .. spec.name)

  -- Track the stub lhs in the registry so two lazy specs binding the same
  -- key are flagged, not silently overwriting each other at stub time.
  register_lhs(spec, m, lhs, k.ft)

  local function stub()
    local count = vim.v.count > 0 and tostring(vim.v.count) or ""
    local register = vim.v.register ~= "" and ('"' .. vim.v.register) or ""
    load_spec(spec)  -- install_spec_keys inside overrides this stub with the real mapping
    local replay = vim.api.nvim_replace_termcodes(register .. count .. lhs, true, false, true)
    vim.api.nvim_feedkeys(replay, "m", false)
  end

  if k.ft then
    local fts = type(k.ft) == "table" and k.ft or { k.ft }
    vim.api.nvim_create_autocmd("FileType", {
      pattern = fts,
      callback = function(args)
        local bopts = vim.tbl_extend("force", opts, { buffer = args.buf })
        vim.keymap.set(m, lhs, stub, bopts)
      end,
    })
  else
    vim.keymap.set(m, lhs, stub, opts)
  end
end

local function install_spec_stubs(spec)
  if not spec.keys then return end
  for _, k in ipairs(spec.keys) do
    for_each_mode(k, function(m) set_stub_keymap(spec, k, m) end)
  end
end

local function install_all(specs)
  -- Build vim.pack.add payload; skip dev plugins.
  local add_list = {}
  for _, s in ipairs(specs) do
    if not s.dev then
      add_list[#add_list + 1] = {
        src = s.src, name = s.name, version = s.version,
      }
    end
  end
  if #add_list > 0 and vim.pack and vim.pack.add then
    -- load = no-op function: vim.pack installs plugins to disk but does NOT
    -- add them to runtimepath. Without this, vim.pack calls :packadd! which
    -- adds every plugin to rtp, and Neovim's post-init plugin-loading phase
    -- then sources every plugin/ file in rtp — defeating lazy loading.
    -- We take over rtp management via our own load_spec/packadd flow.
    local ok, err = pcall(vim.pack.add, add_list, { confirm = false, load = function() end })
    if not ok then notify("vim.pack.add: " .. err, vim.log.levels.ERROR) end
  end
end

-- Run `build` command after vim.pack installs or updates a plugin. The build
-- runs synchronously in the plugin's install directory. Fires only on install
-- or update (not on regular load).
local function register_build_hooks()
  vim.api.nvim_create_autocmd("PackChanged", {
    group = vim.api.nvim_create_augroup("core.pack.build", { clear = true }),
    callback = function(args)
      local data = args.data
      if not data or (data.kind ~= "install" and data.kind ~= "update") then return end
      local spec = data.spec and M._specs[data.spec.name]
      if not spec or type(spec.build) ~= "string" or spec.build == "" then return end
      notify(("core.pack: building %s..."):format(spec.name), vim.log.levels.INFO)
      local result = vim.system({ "sh", "-c", spec.build }, { cwd = data.path, text = true }):wait()
      if result.code == 0 then
        notify(("core.pack: build ok for %s"):format(spec.name), vim.log.levels.INFO)
      else
        notify(("core.pack: build failed for %s: %s"):format(spec.name, result.stderr or ""), vim.log.levels.ERROR)
      end
    end,
  })
end

-- Idempotent-destructive: calling setup() again resets all registries and
-- re-runs init hooks. Safe for tests that rebuild a fresh pack state, not
-- safe to call mid-session after plugins have loaded.
function M.setup(cfg)
  cfg = cfg or {}
  local specs = cfg.specs or {}

  M._specs = {}
  M._loaded = {}
  M._opts = {}
  M._key_registry = {}
  M._warned_conflicts = {}
  -- NOTE: do NOT reset M._on_load here. Plugin spec files call
  -- Lib.plugin.on_load(...) as side effects at require-time, and
  -- require("config.plugins") runs as setup's argument — before setup's
  -- body. Resetting would wipe hooks that the caller just registered.
  M._on_load = M._on_load or {}

  local ordered = {}
  for _, raw in ipairs(specs) do
    local spec = normalize(raw)
    if spec.enabled and (spec.cond == nil or (type(spec.cond) == "function" and spec.cond()) or spec.cond == true) then
      M._specs[spec.name] = spec
      M._opts[spec.name] = spec.opts
      ordered[#ordered + 1] = spec
      if spec.init then
        local ok, err = xpcall(spec.init, debug.traceback, spec)
        if not ok then notify(("init(%s): %s"):format(spec.name, err), vim.log.levels.ERROR) end
      end
    end
  end

  -- Register build hook BEFORE install_all so PackChanged fires fire during installs.
  register_build_hooks()
  install_all(ordered)

  -- Reproducibility is handled by Neovim's built-in vim.pack lockfile at
  -- $XDG_CONFIG_HOME/nvim/nvim-pack-lock.json — auto-written on every
  -- vim.pack.add / update, consulted on next boot.

  -- Apply the install-time colorscheme up-front so any subsequent code
  -- (custom chrome's apply_hl, etc.) samples themed highlights. Matches
  -- lazy.nvim's `install.colorscheme`. When the colorscheme is a known
  -- spec we run its full config (so opts like flavour apply), then mark
  -- it loaded so the eager-load phase skips a redundant re-apply.
  if cfg.install and cfg.install.colorscheme then
    local cs = cfg.install.colorscheme
    local spec = M._specs[cs]
    if spec then
      load_spec(spec)
    else
      pcall(vim.cmd.packadd,     cs)
      pcall(vim.cmd.colorscheme, cs)
    end
  end

  -- Eager load by priority desc
  local eagers = {}
  for _, s in ipairs(ordered) do if not s.lazy then eagers[#eagers + 1] = s end end
  table.sort(eagers, function(a, b) return a.priority > b.priority end)
  for _, s in ipairs(eagers) do load_spec(s) end

  M._register_triggers(ordered)
end

-- Deferred re-fire of a lazy-trigger event, so plugin autocmds registered
-- during load_spec see the current buffer.
--
-- Grouping in _register_triggers (one autocmd per event, not per spec)
-- prevents fan-out at the source: N specs sharing BufWritePre produce 1
-- re-fire, not N. The dedup/guard/retry below are defense-in-depth for
-- edge cases:
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
M._refire_pending = {}  -- scheduled, not yet run
M._refire_active  = {}  -- currently running
M._refire_retry   = {}  -- request made while active: latest opts to replay
local _refire_pending = M._refire_pending
local _refire_active  = M._refire_active
local _refire_retry   = M._refire_retry
local schedule_refire
schedule_refire = function(event, opts)
  local key = event .. "\0" .. (opts.buffer or 0) .. "\0" .. (opts.pattern or "")
  if _refire_active[key] then
    -- Park the request — we'll replay once active clears. Latest-wins is
    -- correct for lazy-load: each call carries opts from one original
    -- event fire, and the re-fire just needs to give handlers a chance
    -- to catch up on the current state.
    _refire_retry[key] = { event = event, opts = opts }
    return
  end
  if _refire_pending[key] then return end
  _refire_pending[key] = true
  vim.schedule(function()
    _refire_pending[key] = nil
    _refire_active[key] = true
    local ok, err = pcall(vim.api.nvim_exec_autocmds, event, opts)
    _refire_active[key] = nil
    if not ok then
      notify(("core.pack refire(%s): %s"):format(event, err), vim.log.levels.ERROR)
    end
    local retry = _refire_retry[key]
    if retry then
      _refire_retry[key] = nil
      schedule_refire(retry.event, retry.opts)
    end
  end)
end
M._schedule_refire = schedule_refire  -- exposed for tests

function M._register_triggers(specs)
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

function M.load(name)
  local spec = M._specs[name]
  if not spec then return end
  load_spec(spec)
end

function M.has(name)    return M._specs[name] ~= nil end
function M.loaded(name) return M._loaded[name] == true end
function M.opts(name)   return M._opts[name] or {} end

function M.on_load(name, fn)
  if M._loaded[name] then fn(); return end
  M._on_load[name] = M._on_load[name] or {}
  table.insert(M._on_load[name], fn)
end

-- Add extra keymaps to an already-registered spec without editing spec.keys.
-- If the plugin is loaded, installs immediately. Otherwise defers to on_load
-- so the mapping is installed when the plugin loads via its own trigger.
--
-- Note: unlike spec.keys, add_keys does NOT install a stub on lazy plugins.
-- To make a key load the plugin, put it in spec.keys. add_keys is for
-- augmenting — user overrides from after/, per-project rebindings, etc.
function M.add_keys(name, keys)
  local spec = M._specs[name]
  if not spec then
    notify(("core.pack: add_keys unknown spec '%s'"):format(name), vim.log.levels.WARN)
    return
  end
  local function install()
    for _, k in ipairs(keys) do
      for_each_mode(k, function(m)
        set_real_keymap(spec, k, m)
        validate_plug(spec, k, m)
      end)
    end
  end
  if M._loaded[name] then install() else M.on_load(name, install) end
end

function M._status_lines()
  local names = vim.tbl_keys(M._specs)
  table.sort(names)
  local lines = { string.format("Packs: %d registered", #names), "" }
  for _, n in ipairs(names) do
    local s = M._specs[n]
    local state = M._loaded[n] and "loaded"
        or (s.lazy and "lazy") or "pending"
    local trigger = s.event and ("event=" .. vim.inspect(s.event):gsub("\n%s*", ""))
        or s.ft and ("ft=" .. vim.inspect(s.ft))
        or s.cmd and ("cmd=" .. vim.inspect(s.cmd))
        or s.keys and "keys"
        or ("priority=" .. s.priority)
    lines[#lines + 1] = string.format("  %-30s  %-10s  %s", n, state, trigger)
  end
  return lines
end

vim.api.nvim_create_user_command("PackStatus", function()
  for _, l in ipairs(M._status_lines()) do print(l) end
end, { desc = "List registered plugin specs" })

vim.api.nvim_create_user_command("PackUpdate", function(opts)
  if not (vim.pack and vim.pack.update) then
    notify("vim.pack.update not available", vim.log.levels.ERROR); return
  end
  local args = opts.fargs
  local names = #args > 0 and args or nil
  vim.pack.update(names)
  -- nvim-pack-lock.json is written by vim.pack.update itself; no extra
  -- refresh needed here.
end, { nargs = "*", desc = "Update plugin(s)" })

vim.api.nvim_create_user_command("PackClean", function()
  if not (vim.pack and vim.pack.get and vim.pack.del) then
    notify("vim.pack.del not available", vim.log.levels.ERROR); return
  end
  local installed = vim.pack.get() or {}
  local to_remove = {}
  for _, info in ipairs(installed) do
    if not M._specs[info.spec.name] then to_remove[#to_remove + 1] = info.spec.name end
  end
  if #to_remove == 0 then print("Nothing to clean"); return end
  vim.pack.del(to_remove)
  print("Removed: " .. table.concat(to_remove, ", "))
end, { desc = "Remove plugins not in spec" })

vim.api.nvim_create_user_command("PackSync", function()
  vim.cmd("PackUpdate")
  vim.cmd("PackClean")
end, { desc = "Update then clean" })

return M
