-- lua/core/pack.lua
local M = {}

M._specs = {}       -- { [name] = normalized_spec }
M._loaded = {}      -- { [name] = true }
M._opts = {}        -- { [name] = merged_opts }
M._on_load = {}     -- { [name] = { fn, ... } }

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
      vim.notify(("core.pack: unknown field '%s' on spec '%s'"):format(k, name), vim.log.levels.WARN)
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
  -- Invariant: spec.name matches the vim.pack install directory under pack/core/opt/<name>.
  require("core.profile").span("packadd:" .. spec.name, "packadd", function()
    local ok, err = pcall(vim.cmd, "packadd " .. spec.name)
    if not ok then
      vim.notify(("core.pack packadd(%s) failed: %s"):format(spec.name, err), vim.log.levels.ERROR)
    end
  end)
end

local _loading = {}  -- cycle-detection guard: names currently mid-load

local function load_spec(spec)
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
  _loading[spec.name] = nil
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
    if not ok then vim.notify("vim.pack.add: " .. err, vim.log.levels.ERROR) end
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
      vim.notify(("core.pack: building %s..."):format(spec.name), vim.log.levels.INFO)
      local result = vim.system({ "sh", "-c", spec.build }, { cwd = data.path, text = true }):wait()
      if result.code == 0 then
        vim.notify(("core.pack: build ok for %s"):format(spec.name), vim.log.levels.INFO)
      else
        vim.notify(("core.pack: build failed for %s: %s"):format(spec.name, result.stderr or ""), vim.log.levels.ERROR)
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
  M._on_load = {}

  local ordered = {}
  for _, raw in ipairs(specs) do
    local spec = normalize(raw)
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

function M._register_triggers(specs)
  local event = require("core.event")

  for _, spec in ipairs(specs) do
    -- `keys` registration applies to BOTH eager and lazy plugins:
    -- eager plugins still want their spec.keys bound (just without a load wrapper).
    -- `event/ft/cmd` only apply to lazy plugins (nothing to trigger for eager).
    if spec.lazy and spec.event then
      local events = event.expand(spec.event)
      local real, user = {}, {}
      for _, e in ipairs(events) do
        if e == "VeryLazy" then
          table.insert(user, "VeryLazy")
        elseif e:match("^User ") then
          -- "User Pattern"
          local pat = e:sub(6)
          vim.api.nvim_create_autocmd("User", {
            once = true, pattern = pat,
            callback = function(args)
              load_spec(spec)
              -- Defer re-fire to next tick to break autocmd nesting.
              vim.schedule(function()
                vim.api.nvim_exec_autocmds("User", { pattern = pat, data = args.data, modeline = false })
              end)
            end,
          })
        else
          table.insert(real, e)
        end
      end
      if #real > 0 then
        vim.api.nvim_create_autocmd(real, {
          once = true,
          callback = function(args)
            load_spec(spec)
            -- Defer re-fire to break nesting. When multiple once-autocmds
            -- trigger on the same event, synchronous re-fire stacks them
            -- and the plugin's newly-registered handlers re-trigger further
            -- autocmds -> E218: Autocommand nesting too deep. vim.schedule
            -- returns to the event loop, avoiding the recursion.
            vim.schedule(function()
              vim.api.nvim_exec_autocmds(args.event, {
                buffer = args.buf,
                data = args.data,
                modeline = false,
              })
            end)
          end,
        })
      end
      for _, p in ipairs(user) do
        if p == "VeryLazy" then
          event.on("VeryLazy", function() load_spec(spec) end)
        end
      end
    end

    -- ft (lazy only)
    if spec.lazy and spec.ft then
      local fts = type(spec.ft) == "table" and spec.ft or { spec.ft }
      vim.api.nvim_create_autocmd("FileType", {
        once = true, pattern = fts,
        callback = function(args)
          load_spec(spec)
          vim.schedule(function()
            vim.api.nvim_exec_autocmds("FileType", { buffer = args.buf, modeline = false })
          end)
        end,
      })
    end

    -- cmd (lazy only)
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

    -- keys
    -- Three forms supported:
    --   { "<lhs>" }                        => trigger only; plugin registers the real keymap in config()
    --   { "<lhs>", function()...end, ... } => trigger + handler; we register a wrapper that loads + invokes
    --   { "<lhs>", "<rhs>", ... }          => trigger + string rhs; we register a wrapper that loads + feeds
    --
    -- `ft` on a key entry scopes the mapping to matching filetypes via a
    -- buffer-local keymap registered on FileType. Without this, per-ft
    -- keys (e.g. flutter's <Leader>cr) would shadow global bindings
    -- everywhere.
    if spec.keys then
      for _, k in ipairs(spec.keys) do
        local lhs = k[1] or k.lhs
        local rhs = k[2]
        local mode = k.mode or "n"
        local modes = type(mode) == "table" and mode or { mode }
        local map_opts = {
          desc = k.desc,
          silent = k.silent ~= false,
          noremap = k.noremap,
          nowait = k.nowait,
          expr = k.expr,
          buffer = k.buffer,
          remap = k.remap,
        }

        local function build_mapping(m)
          if type(rhs) == "function" then
            return function() load_spec(spec); return rhs() end, map_opts
          elseif type(rhs) == "string" then
            local expr_opts = vim.tbl_extend("force", map_opts, { expr = true })
            return function()
              load_spec(spec)
              return vim.api.nvim_replace_termcodes(rhs, true, false, true)
            end, expr_opts
          end
          return function()
            local count = vim.v.count > 0 and tostring(vim.v.count) or ""
            local register = vim.v.register ~= "" and ('"' .. vim.v.register) or ""
            load_spec(spec)
            pcall(vim.keymap.del, m, lhs)
            local replay = vim.api.nvim_replace_termcodes(register .. count .. lhs, true, false, true)
            vim.api.nvim_feedkeys(replay, "m", false)
          end, vim.tbl_extend("force", map_opts, { desc = map_opts.desc or ("lazy: " .. spec.name) })
        end

        for _, m in ipairs(modes) do
          local fn, opts = build_mapping(m)
          if k.ft then
            -- Buffer-local keymap installed on FileType match. Guarded
            -- against re-registration on repeated BufEnter events.
            local fts = type(k.ft) == "table" and k.ft or { k.ft }
            vim.api.nvim_create_autocmd("FileType", {
              pattern = fts,
              callback = function(args)
                local buf_opts = vim.tbl_extend("force", opts, { buffer = args.buf })
                vim.keymap.set(m, lhs, fn, buf_opts)
              end,
            })
          else
            vim.keymap.set(m, lhs, fn, opts)
          end
        end
      end
    end
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
    vim.notify("vim.pack.update not available", vim.log.levels.ERROR); return
  end
  local args = opts.fargs
  local names = #args > 0 and args or nil
  vim.pack.update(names)
  -- nvim-pack-lock.json is written by vim.pack.update itself; no extra
  -- refresh needed here.
end, { nargs = "*", desc = "Update plugin(s)" })

vim.api.nvim_create_user_command("PackClean", function()
  if not (vim.pack and vim.pack.get and vim.pack.del) then
    vim.notify("vim.pack.del not available", vim.log.levels.ERROR); return
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
