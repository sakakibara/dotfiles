-- lua/core/pack.lua
local M = {}

local Spec = require("core.pack.spec")

M._specs = {}       -- { [name] = normalized_spec }
M._loaded = {}      -- { [name] = true }
M._opts = {}        -- { [name] = merged_opts }
M._on_load = {}     -- { [name] = { fn, ... } }
M._key_registry = {} -- { [mode..":"..lhs..":"..(ft or "")] = spec.name }
M._warned_conflicts = {} -- { [sig..":"..nameA..":"..nameB (sorted)] = true }
M._installed_global = {} -- { [mode..":"..lhs] = spec.name } — global maps we set
M._warned_external = {}  -- { [mode..":"..lhs] = true } — already-warned externals
M._warned_external_ft = {} -- { [mode..":"..lhs..":"..bufnr] = true } — ft-scoped per-buffer dedup

-- Notify with guaranteed :messages persistence. config/init.lua's wrapper A
-- normally tees vim.notify into :messages history, but snacks.notifier loads
-- eagerly inside pack.setup and replaces vim.notify with its own (toast-only,
-- no :messages echo) — so any warning emitted from this file *after* snacks
-- loads would otherwise vanish from history. Echo to :messages directly here
-- in addition to calling vim.notify, regardless of which provider currently
-- owns it.
local function notify(msg, level)
  level = level or vim.log.levels.INFO
  local hl = (level >= vim.log.levels.ERROR and "ErrorMsg")
    or (level >= vim.log.levels.WARN and "WarningMsg")
    or "Normal"
  pcall(vim.api.nvim_echo, { { tostring(msg), hl } }, true, {})
  vim.notify(msg, level)
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

-- Copy `existing` (a maparg(...,true) table) to `target_lhs` in `mode`.
-- buf=nil installs globally; buf=<bufnr> installs buffer-locally (ft-scoped).
-- Returns true if installation happened, false otherwise (caller decides
-- whether the failure is fatal — for `preserve`, it isn't: the spec's own
-- key still gets installed).
local function preserve_mapping(existing, mode, target_lhs, source_lhs, buf)
  -- Preflight: don't clobber an already-bound target. For ft-scoped (buf
  -- non-nil), maparg without buffer context returns the global mapping
  -- regardless — but ft-scoped install via vim.keymap.set with buffer = buf
  -- does not collide with the global, so the preflight uses the appropriate
  -- scope: buffer-local maparg if buf is set.
  local existing_target = (buf ~= nil)
    and vim.fn.maparg(target_lhs, mode, false, true)  -- buffer context: caller (FileType cb) is in that buf
    or vim.fn.maparg(target_lhs, mode)
  local target_busy = false
  if buf == nil then
    target_busy = type(existing_target) == "string" and existing_target ~= ""
  else
    -- maparg returns buffer=1 as a flag (means "is buffer-local"), not the actual bufnr.
    target_busy = type(existing_target) == "table" and not vim.tbl_isempty(existing_target)
      and existing_target.buffer == 1
  end
  if target_busy then
    notify(
      ("core.pack: '%s' preserve target already in use (mode=%s)"):format(target_lhs, mode),
      vim.log.levels.WARN)
    return false
  end

  if not (existing.callback or (existing.rhs and existing.rhs ~= "")) then
    notify(
      ("core.pack: cannot preserve '%s' (no rhs/callback)"):format(source_lhs),
      vim.log.levels.WARN)
    return false
  end
  local opts = {
    desc    = existing.desc or ("preserved: " .. source_lhs),
    expr    = existing.expr == 1,
    silent  = existing.silent == 1,
    nowait  = existing.nowait == 1,
    noremap = existing.noremap == 1,
    script  = existing.script == 1,
    buffer  = buf,
  }
  local rhs = existing.callback or existing.rhs
  local ok, err = pcall(vim.keymap.set, mode, target_lhs, rhs, opts)
  if not ok then
    notify(
      ("core.pack: cannot preserve '%s' to '%s': %s")
        :format(source_lhs, target_lhs, tostring(err)),
      vim.log.levels.WARN)
    return false
  end
  return true
end

-- ft-scoped collision dispatch. Mirrors register_lhs's external-block tail
-- but scoped to a specific buffer. All maparg calls happen inside
-- nvim_buf_call so buffer-local mappings are visible. Per-buffer dedup
-- uses _warned_external_ft.
local function apply_collision_for_buf(spec, k, mode, lhs, buf)
  local key = mode .. ":" .. lhs .. ":" .. tostring(buf)
  if M._warned_external_ft[key] then return end
  -- Run the entire dispatch inside buf's context so maparg sees its local
  -- mappings. maparg returns buffer=1 (not the actual bufnr) when the
  -- mapping is buffer-local.
  pcall(vim.api.nvim_buf_call, buf, function()
    local existing = vim.fn.maparg(lhs, mode, false, true)
    if not existing or vim.tbl_isempty(existing) then return end
    if existing.buffer ~= 1 then return end  -- not buffer-local for this buf
    M._warned_external_ft[key] = true
    -- Validation here mirrors register_lhs's; for ft we skip the more-elaborate
    -- combinations and only apply preserve/override directly.
    if k.override then return end
    if type(k.preserve) == "string" and k.preserve ~= lhs then
      preserve_mapping(existing, mode, k.preserve, lhs, buf)
      return
    end
    local where = (existing.desc and existing.desc ~= "") and existing.desc
      or (existing.rhs and existing.rhs ~= "" and existing.rhs:sub(1, 60))
      or "<lua callback>"
    notify(
      ("core.pack: '%s' (mode=%s, ft=%s, buf=%d) on '%s' overrides existing mapping (was: %s)")
        :format(lhs, mode, tostring(k.ft), buf, spec.name, where),
      vim.log.levels.WARN)
  end)
end

local function register_lhs(spec, mode, lhs, ft, k)
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

  -- External collision check: another mapping (from a plugin's setup() or
  -- from config/keymaps.lua) already binds this lhs. The spec-vs-spec check
  -- above only sees other plugin specs going through core.pack; this catches
  -- everything else.
  --
  -- Skipped for ft-scoped specs because they install per-buffer at FileType
  -- time (the mapping isn't global yet at registration), and for keys with
  -- rhs == nil (keymap is owned by the plugin's own setup, no install on
  -- our side). False-positive guards: skip when the existing mapping was
  -- installed by core.pack itself — either via _installed_global (real
  -- mappings from set_real_keymap) or via the desc prefix used by both
  -- set_real_keymap ("key: ") and set_stub_keymap ("lazy: ").
  if ft then return end
  local ext_key = mode .. ":" .. lhs
  if M._warned_external[ext_key] then return end
  if M._installed_global[ext_key] == spec.name then return end

  -- Validate collision opts. Bad combinations get one warning and the
  -- offending field is dropped; processing continues.
  local k_local = k and vim.deepcopy(k) or nil
  if k_local and k_local.preserve ~= nil and type(k_local.preserve) ~= "string" then
    notify(
      ("core.pack: invalid preserve on '%s' (mode=%s); expected string lhs"):format(lhs, mode),
      vim.log.levels.WARN)
    k_local.preserve = nil
  end
  if k_local and k_local.override ~= nil and type(k_local.override) ~= "boolean" then
    notify(
      ("core.pack: invalid override on '%s' (mode=%s); expected boolean"):format(lhs, mode),
      vim.log.levels.WARN)
    k_local.override = nil
  end
  if k_local and k_local.preserve and k_local.override then
    notify(
      ("core.pack: preserve and override are mutually exclusive on '%s' (mode=%s); using preserve"):format(lhs, mode),
      vim.log.levels.WARN)
    k_local.override = nil
  end
  if k_local and k_local.preserve == lhs then
    notify(
      ("core.pack: preserve target equals source on '%s' (mode=%s); ignoring"):format(lhs, mode),
      vim.log.levels.WARN)
    k_local.preserve = nil
  end

  local existing = vim.fn.maparg(lhs, mode, false, true)
  if not existing or vim.tbl_isempty(existing) then return end
  if existing.buffer ~= 0 then return end
  if existing.desc and (existing.desc:find("^key: ") or existing.desc:find("^lazy: ")) then return end
  M._warned_external[ext_key] = true  -- sentinel covers all branches below

  if k_local and k_local.override then
    return
  end

  if k_local and type(k_local.preserve) == "string" then
    preserve_mapping(existing, mode, k_local.preserve, lhs, nil)
    return
  end

  local where = (existing.desc and existing.desc ~= "") and existing.desc
    or (existing.rhs and existing.rhs ~= "" and existing.rhs:sub(1, 60))
    or "<lua callback>"
  notify(
    ("core.pack: '%s' (mode=%s) on '%s' overrides existing mapping (was: %s)")
      :format(lhs, mode, spec.name, where),
    vim.log.levels.WARN)
end

-- Install one real keymap for (spec, k, mode). Handles ft-scoped (FileType
-- autocmd + scan of already-loaded matching buffers) and global mappings.
local function set_real_keymap(spec, k, m)
  local lhs = k[1] or k.lhs
  local rhs = k[2]
  local opts = base_map_opts(k)
  opts.remap = remap_default(k)
  opts.desc  = opts.desc or ("key: " .. spec.name)

  register_lhs(spec, m, lhs, k.ft, k)

  -- rhs==nil means "plugin's own setup owns the keymap" — we only track it
  -- for conflict detection and skip installing anything.
  if rhs == nil then return end

  if k.ft then
    local fts = type(k.ft) == "table" and k.ft or { k.ft }
    local function install_on_buf(args)
      apply_collision_for_buf(spec, k, m, lhs, args.buf)
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
    M._installed_global[m .. ":" .. lhs] = spec.name
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

-- Normalize a plugin/module name to a canonical form so dash/underscore and
-- nvim-prefix differences compare equal: e.g. "better-escape" and
-- "better_escape" both → "better_escape"; "nvim-treesitter-context" and
-- "treesitter-context" both → "treesitter_context". Mirrors lazy.nvim's
-- find_main heuristic.
local function _norm_modname(s)
  return s:lower()
    :gsub("nvim", "")
    :gsub("[%.%-]", "_")
    :gsub("__+", "_")
    :gsub("^_+", "")
    :gsub("_+$", "")
end

-- Resolve the plugin's Lua module name. Order:
--   1. spec.main (explicit override)
--   2. derived from name (strip ".nvim" suffix) — works for the common case
--   3. scan the plugin's lua/ directory for a top-level module whose
--      normalized name matches the plugin's normalized name
-- Exposed as M._resolve_main for tests.
function M._resolve_main(spec)
  if spec.main then return spec.main end
  local derived = spec.name:gsub("%.nvim$", "")
  local lua_dir = require("core.pack.install").install_dir(spec.name) .. "/lua"
  if vim.fn.isdirectory(lua_dir) == 0 then return derived end
  -- Quick check: does the derived name exist as a directory or .lua file?
  if vim.fn.isdirectory(lua_dir .. "/" .. derived) == 1
     or vim.fn.filereadable(lua_dir .. "/" .. derived .. ".lua") == 1 then
    return derived
  end
  -- Fall back: scan and match by normalized name.
  local target = _norm_modname(derived)
  for _, entry in ipairs(vim.fn.readdir(lua_dir)) do
    local mod = entry:gsub("%.lua$", "")
    if _norm_modname(mod) == target then return mod end
  end
  return derived
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
        local mod = M._resolve_main(spec)
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
  -- Invariant: spec.name matches the install directory under pack/core/opt/<name>.
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
  register_lhs(spec, m, lhs, k.ft, k)

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
        apply_collision_for_buf(spec, k, m, lhs, args.buf)
        local bopts = vim.tbl_extend("force", opts, { buffer = args.buf })
        vim.keymap.set(m, lhs, stub, bopts)
      end,
    })
  else
    vim.keymap.set(m, lhs, stub, opts)
    M._installed_global[m .. ":" .. lhs] = spec.name
  end
end

local function install_spec_stubs(spec)
  if not spec.keys then return end
  for _, k in ipairs(spec.keys) do
    for_each_mode(k, function(m) set_stub_keymap(spec, k, m) end)
  end
end

local function install_all(specs)
  local Lock    = require("core.pack.lock")
  local Install = require("core.pack.install")

  -- One-shot migration from the old lockfile (no-op if our lockfile already populated).
  Lock.migrate_from_vim_pack()

  -- For specs already on disk, packadd; for those not, install in parallel.
  local to_install = {}
  for _, s in ipairs(specs) do
    if not s.dev and vim.fn.isdirectory(Install.install_dir(s.name)) == 0 then
      to_install[#to_install + 1] = s
    end
  end
  local failed = {}
  local splash = nil  -- returned to caller so it can transition to the
                       -- setup phase and close at VeryLazy
  if #to_install > 0 then
    -- Block setup() on install completion so the eager load phase below
    -- finds every spec on disk. Without the wait, a fresh state dir means
    -- load_spec calls run_config → require("<plugin>") → "module not found"
    -- and packadd → E919, until the user manually restarts. vim.wait pumps
    -- the event loop so clone jobs progress and the install fidget updates.
    --
    -- Cold-install splash: a centered "first-run install" indicator that
    -- replaces the otherwise-blank screen during the wait. Updates from
    -- on_progress; the splash is NOT closed here — the caller transitions
    -- it to the setup phase and closes it at VeryLazy so it covers eager
    -- loads too (otherwise nvim looks frozen for several seconds with no
    -- visual indicator of activity).
    local UI = require("core.pack.ui")
    splash = UI.cold_install_splash(#to_install)
    local done = false
    local ok, err = pcall(Install.install_missing, to_install, {
      -- Splash takes over the prominent indicator role; the corner fidget
      -- would just be a redundant second progress widget on cold start.
      -- :PackInstall (user-invoked) still gets the fidget.
      open_window = false,
      on_failed = function(name, _msg) failed[name] = true end,
      on_progress = function(d, _t, _last) splash:update(d) end,
      on_complete = function()
        local installed_count = #to_install - vim.tbl_count(failed)
        -- Deferred so the notify call runs after eager loads complete and
        -- snacks owns vim.notify — it then renders as a single toast
        -- (top-right) instead of going through the wrapper-A cmdline echo
        -- path during the splash → snacks-load transition.
        vim.schedule(function()
          vim.notify(
            ("core.pack: installed %d plugins"):format(installed_count),
            vim.log.levels.INFO)
        end)
        done = true
      end,
    })
    if not ok then
      splash:close(); splash = nil
      notify("core.pack: install failed: " .. tostring(err), vim.log.levels.ERROR)
      return {}, nil  -- treat as no failures from a registry-pruning standpoint
    end
    vim.wait(180000, function() return done end, 50)
  end
  local out = {}
  for name in pairs(failed) do out[#out + 1] = name end
  return out, splash
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
  M._installed_global = {}
  M._warned_external = {}
  M._warned_external_ft = {}
  -- NOTE: do NOT reset M._on_load here. Plugin spec files call
  -- Lib.plugin.on_load(...) as side effects at require-time, and
  -- require("config.plugins") runs as setup's argument — before setup's
  -- body. Resetting would wipe hooks that the caller just registered.
  M._on_load = M._on_load or {}

  local resolved, warnings = Spec.resolve(specs)
  for _, w in ipairs(warnings) do
    notify("core.pack: " .. w, vim.log.levels.WARN)
  end
  local ordered = {}
  for _, spec in ipairs(resolved) do
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

  local failed_names, splash = install_all(ordered)
  local failed = {}
  for _, name in ipairs(failed_names) do failed[name] = true end
  for name in pairs(failed) do
    M._specs[name] = nil
    M._opts[name] = nil
  end
  local pruned = {}
  for _, s in ipairs(ordered) do
    if M._specs[s.name] then pruned[#pruned + 1] = s end
  end
  ordered = pruned

  -- Splash transitions from install phase to setup phase. The eager
  -- load loop below is synchronous and blocks the UI; without the
  -- splash covering it, nvim looks frozen for several seconds while
  -- plugin configs run. Splash stays up through eager loads, animates
  -- a per-plugin name in the spinner, and closes at VeryLazy when
  -- setup is fully complete.
  if splash then splash:enter_setup_phase() end

  -- Reproducibility: lockfile at $XDG_CONFIG_HOME/nvim/pack-lock.json,
  -- written incrementally by core.pack.install on each install/update,
  -- read by Install.update to diff plugin revisions.

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
  for i, s in ipairs(eagers) do
    if splash then splash:set_setup_status(i, #eagers, s.name) end
    load_spec(s)
  end

  M._register_triggers(ordered)

  -- Close the splash either on first user input OR after a stretch of
  -- "no status updates" (idle). The idle timer is preferable to a fixed
  -- timeout because it keeps the splash up *exactly* as long as something
  -- (eager loads, then treesitter install via the rerouted logger) is
  -- still reporting progress, and closes promptly once the activity dies
  -- down — independent of whether the actual install took 5s or 60s.
  if splash then
    -- Splash close is driven by treesitter's install task: when the
    -- install Task signals completion, we close. This makes the splash
    -- cover the *entire* cold-install timeline (clones → eager loads →
    -- treesitter parser downloads + compiles), since treesitter is the
    -- last and longest piece of cold-install work.
    --
    -- The treesitter spec's `config()` registers a callback via
    -- `task:await(...)` that calls splash:close() — see plugins/treesitter.lua.
    -- We only register an on_key here so the user can dismiss the splash
    -- at will, plus a far-out safety fallback (5 minutes) so a hung or
    -- never-completing install can't strand the user forever.
    vim.api.nvim_create_autocmd("User", {
      pattern = "VeryLazy",
      once = true,
      callback = function()
        local key_handler
        key_handler = vim.on_key(function()
          vim.schedule(function() splash:close() end)
          vim.on_key(nil, key_handler)
        end)
        vim.defer_fn(function() splash:close() end, 300000)
      end,
    })
  end
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

function M._structured_status(filter_pattern)
  local Profile = require("core.profile")
  local names = vim.tbl_keys(M._specs)
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
    if M._loaded[n] then loaded_count = loaded_count + 1
    elseif M._specs[n].lazy then lazy_count = lazy_count + 1 end
  end

  -- Name column: longest actual name, floor 16, cap 50.
  local name_max = 16
  for _, n in ipairs(names) do name_max = math.max(name_max, #n) end
  if name_max > 50 then name_max = 50 end

  local LOAD_W = 9

  -- Trigger column gets remainder. Fixed prefix bytes:
  --   2 (lead) + 4 (padded glyph) + 2 + name_max + 2 + 8 (state) + 2 + 9 (load) + 2 = 29 + name_max
  -- Use a generous default of 180 chars since :PackStatus renders before the
  -- window is open and can't know the actual width at that point.
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
    local s = M._specs[n]
    local state, glyph, glyph_hl
    if M._loaded[n] then
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

vim.api.nvim_create_user_command("PackStatus", function()
  local UI = require("core.pack.ui")
  local function render(filter_pattern)
    local data = M._structured_status(filter_pattern)
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
  for _, s in pairs(M._specs) do specs[#specs + 1] = s end
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
  for _, s in pairs(M._specs) do specs[#specs + 1] = s end
  local target = opts.bang and "lockfile" or "remote"
  Install.update(specs, opts.fargs, { confirm = true, target = target })
end, {
  bang = true,
  nargs = "*",
  complete = function(arglead)
    local out = {}
    for name in pairs(M._specs) do
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
  for _, s in pairs(M._specs) do specs[#specs + 1] = s end
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
  for _, s in pairs(M._specs) do
    if s.build and (target == nil or s.name == target) then total = total + 1 end
  end
  if total == 0 then
    notify(target and ("core.pack: no build hook for " .. target) or "core.pack: no plugins with build hooks")
    fidget:close()
    return
  end
  fidget:set_status("core.pack", ("rebuilding 0/%d"):format(total))
  local done = 0
  for _, s in pairs(M._specs) do
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
    for name, s in pairs(M._specs) do
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
  if not M.has(name) then notify(("core.pack: unknown plugin '%s'"):format(name), vim.log.levels.WARN); return end
  if M.loaded(name) then notify(("core.pack: %s already loaded"):format(name)); return end
  M.load(name)
  notify(("core.pack: loaded %s"):format(name))
end, {
  nargs = 1,
  complete = function(arglead)
    local out = {}
    for n in pairs(M._specs) do
      if not M._loaded[n] and n:lower():find(arglead:lower(), 1, true) then out[#out + 1] = n end
    end
    table.sort(out)
    return out
  end,
  desc = "Manually load a lazy plugin by name",
})

return M
