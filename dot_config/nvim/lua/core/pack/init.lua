-- lua/core/pack/init.lua
local M = {}

local Spec = require("core.pack.spec")

M._specs = {}       -- { [name] = normalized_spec }
M._loaded = {}      -- { [name] = true }
M._opts = {}        -- { [name] = merged_opts }
M._on_load = {}     -- { [name] = { fn, ... } }

-- Forward declaration: load_spec is defined later (after run_config/packadd
-- helpers) but needs to be referenced earlier by the keymaps stub callback.
-- Closure-captured by reference — assignment below populates the upvalue.
local load_spec

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
      -- lazy.nvim parity: opts may be a table OR a function returning a
      -- table. Resolve here (not at registration time) so the function
      -- can safely require plugin modules that aren't on rtp until
      -- packadd above. Inside xpcall so opts() errors flow through the
      -- same notify path as setup() errors.
      local opts = spec.opts
      if type(opts) == "function" then opts = opts(spec, {}) or {} end
      if spec.config then
        spec.config(spec, opts)
      else
        local mod = M._resolve_main(spec)
        local ok2, plugin = pcall(require, mod)
        if ok2 and type(plugin) == "table" and type(plugin.setup) == "function" then
          plugin.setup(opts)
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
    vim.notify(("core.pack config(%s): %s"):format(spec.name, err), vim.log.levels.ERROR)
  end

  for _, fn in ipairs(M._on_load[spec.name] or {}) do
    local okfn, errfn = xpcall(fn, debug.traceback)
    if not okfn then vim.notify(("on_load(%s): %s"):format(spec.name, errfn), vim.log.levels.ERROR) end
  end
  M._on_load[spec.name] = nil
end

local function packadd(spec)
  if spec.dev then return end  -- dev plugins: assume on rtp already or skip for tests
  -- Invariant: spec.name matches the install directory under pack/core/opt/<name>.
  require("core.profile").span("packadd:" .. spec.name, "packadd", function()
    local ok, err = pcall(vim.cmd, "packadd " .. spec.name)
    if not ok then
      vim.notify(("core.pack packadd(%s) failed: %s"):format(spec.name, err), vim.log.levels.ERROR)
    end
  end)
end

-- Keymap installer + lazy stub manager. Closure-binds load_spec by reference
-- (forward-declared at the top), so the stub fired on a lazy-trigger key
-- press calls into load_spec once it's been assigned below.
local Keymaps = require("core.pack.keymaps").create({
  load_spec = function(spec) return load_spec(spec) end,
})

local _loading = {}  -- cycle-detection guard: names currently mid-load

load_spec = function(spec)
  if M._loaded[spec.name] then return end
  if _loading[spec.name] then
    vim.notify(("core.pack: dependency cycle detected at '%s'"):format(spec.name), vim.log.levels.ERROR)
    return
  end
  _loading[spec.name] = true
  for _, dep_name in ipairs(spec.dependencies) do
    local dep = M._specs[dep_name]
    if dep then
      load_spec(dep)
    else
      vim.notify(("core.pack: unknown dependency '%s' for '%s'"):format(dep_name, spec.name), vim.log.levels.WARN)
    end
  end
  packadd(spec)
  run_config(spec)
  -- install_spec_keys runs AFTER run_config so that:
  --   1. any <Plug> mapping the plugin installs in setup() is present when
  --      validate_plug checks it
  --   2. the real keymap overrides any stub that triggered this load, so the
  --      subsequent feedkeys replay (in "m" mode) hits the real mapping.
  Keymaps.install_spec_keys(spec)
  _loading[spec.name] = nil
end


local triggers = require("core.pack.triggers").create({
  load_spec = load_spec,
  install_spec_stubs = Keymaps.install_spec_stubs,
})
M._schedule_refire = triggers.schedule_refire  -- exposed for tests

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
      -- :Pack install (user-invoked) still gets the fidget.
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
      vim.notify("core.pack: install failed: " .. tostring(err), vim.log.levels.ERROR)
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
  -- NOTE: do NOT reset M._on_load here. Plugin spec files call
  -- Lib.plugin.on_load(...) as side effects at require-time, and
  -- require("config.plugins") runs as setup's argument — before setup's
  -- body. Resetting would wipe hooks that the caller just registered.
  M._on_load = M._on_load or {}

  local resolved, warnings = Spec.resolve(specs)
  for _, w in ipairs(warnings) do
    vim.notify("core.pack: " .. w, vim.log.levels.WARN)
  end
  local ordered = {}
  for _, spec in ipairs(resolved) do
    if spec.enabled and (spec.cond == nil or (type(spec.cond) == "function" and spec.cond()) or spec.cond == true) then
      M._specs[spec.name] = spec
      M._opts[spec.name] = spec.opts
      ordered[#ordered + 1] = spec
      if spec.init then
        local ok, err = xpcall(spec.init, debug.traceback, spec)
        if not ok then vim.notify(("init(%s): %s"):format(spec.name, err), vim.log.levels.ERROR) end
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

  triggers.register(ordered)

  -- Backfill `doc/tags` for installed plugins that were cloned before the
  -- per-install helptag generation existed. Idempotent (helptags rewrites
  -- only when needed) and skipped per-plugin when doc/tags already exists.
  vim.schedule(function()
    local Install = require("core.pack.install")
    for _, s in ipairs(ordered) do
      local dir = Install.install_dir(s.name)
      if vim.fn.isdirectory(dir) == 1
         and vim.fn.filereadable(dir .. "/doc/tags") == 0 then
        Install.generate_helptags(dir)
      end
    end
  end)

  -- Close the splash either on first user input OR after a stretch of
  -- "no status updates" (idle). The idle timer is preferable to a fixed
  -- timeout because it keeps the splash up *exactly* as long as something
  -- (eager loads, then treesitter install via the rerouted logger) is
  -- still reporting progress, and closes promptly once the activity dies
  -- down — independent of whether the actual install took 5s or 60s.
  if splash then
    -- Splash close: idle-debounce on status updates (most reliable
    -- "work has stopped" signal we have, given that cold-install work
    -- is spread across many independent async paths), plus on_key as
    -- explicit user dismissal, plus a long safety timeout.
    vim.api.nvim_create_autocmd("User", {
      pattern = "VeryLazy",
      once = true,
      callback = function()
        splash:start_idle_close(5000)
        local key_handler
        key_handler = vim.on_key(function()
          vim.schedule(function() splash:close() end)
          vim.on_key(nil, key_handler)
        end)
        vim.defer_fn(function() splash:close() end, 120000)

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
    vim.notify(("core.pack: add_keys unknown spec '%s'"):format(name), vim.log.levels.WARN)
    return
  end
  local function install()
    -- Reuse install_spec_keys with a synthetic spec carrying just the
    -- augment keys; spec.name is preserved so registry / desc prefixes
    -- still attribute the new mapping to the right plugin.
    Keymaps.install_spec_keys({ name = spec.name, keys = keys })
  end
  if M._loaded[name] then install() else M.on_load(name, install) end
end

require("core.pack.commands").setup(M)

return M
