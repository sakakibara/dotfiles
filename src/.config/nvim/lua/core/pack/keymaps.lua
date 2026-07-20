-- Keymap installation, collision detection, and stub→real transition.
--
-- Two phases: (1) install_spec_keys installs the real mapping after a spec
-- runs run_config — zero wrapper overhead on every subsequent press. (2) For
-- lazy plugins, install_spec_stubs installs a placeholder; on first press
-- the stub loads the plugin (which installs the real mapping via
-- install_spec_keys) and feedkeys ("m" mode) replays the key so the real
-- mapping fires.
--
-- String rhs defaults to remap=true (correct for <Plug>, <Cmd>, etc.); pass
-- remap=false on the key entry to override.

local M = {}

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

-- Build a keymap installer bound to load_spec (called when a stub fires for
-- a lazy spec). Returns { install_spec_keys, install_spec_stubs }. State
-- (registry, warning sentinels) is closure-scoped so multiple Pack
-- instances don't bleed into each other and tests can re-create cleanly.
function M.create(deps)
  local load_spec = assert(deps.load_spec)

  local key_registry      = {}  -- [mode..":"..lhs..":"..(ft or "")] = spec.name
  local warned_conflicts  = {}  -- [sig..":"..nameA..":"..nameB (sorted)] = true
  local installed_global  = {}  -- [mode..":"..lhs] = spec.name — global maps we set
  local warned_external   = {}  -- [mode..":"..lhs] = true — already-warned externals
  local warned_external_ft = {} -- [mode..":"..lhs..":"..bufnr] = true — ft-scoped per-buffer dedup

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
      vim.notify(
        ("core.pack: '%s' preserve target already in use (mode=%s)"):format(target_lhs, mode),
        vim.log.levels.WARN)
      return false
    end

    if not (existing.callback or (existing.rhs and existing.rhs ~= "")) then
      vim.notify(
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
      vim.notify(
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
  -- uses warned_external_ft.
  local function apply_collision_for_buf(spec, k, mode, lhs, buf)
    local key = mode .. ":" .. lhs .. ":" .. tostring(buf)
    if warned_external_ft[key] then return end
    -- Run the entire dispatch inside buf's context so maparg sees its local
    -- mappings. maparg returns buffer=1 (not the actual bufnr) when the
    -- mapping is buffer-local.
    pcall(vim.api.nvim_buf_call, buf, function()
      local existing = vim.fn.maparg(lhs, mode, false, true)
      if not existing or vim.tbl_isempty(existing) then return end
      if existing.buffer ~= 1 then return end  -- not buffer-local for this buf
      warned_external_ft[key] = true
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
      vim.notify(
        ("core.pack: '%s' (mode=%s, ft=%s, buf=%d) on '%s' overrides existing mapping (was: %s)")
          :format(lhs, mode, tostring(k.ft), buf, spec.name, where),
        vim.log.levels.WARN)
    end)
  end

  local function register_lhs(spec, mode, lhs, ft, k)
    local sig = conflict_key(mode, lhs, ft)
    local owner = key_registry[sig]
    if owner and owner ~= spec.name then
      -- Dedup by canonical (sorted) pair so stub→real transitions don't re-warn:
      -- the same pair of specs conflicting on the same lhs is one conflict,
      -- regardless of which spec registered first.
      local a, b = owner, spec.name
      if a > b then a, b = b, a end
      local warn_key = sig .. "|" .. a .. "|" .. b
      if not warned_conflicts[warn_key] then
        warned_conflicts[warn_key] = true
        vim.notify(
          ("core.pack: keymap conflict '%s' (mode=%s, ft=%s): '%s' vs '%s'")
            :format(lhs, mode, tostring(ft or "*"), owner, spec.name),
          vim.log.levels.WARN)
      end
    end
    key_registry[sig] = spec.name

    -- External collision check: another mapping (from a plugin's setup() or
    -- from config/keymaps.lua) already binds this lhs. The spec-vs-spec check
    -- above only sees other plugin specs going through core.pack; this catches
    -- everything else.
    --
    -- Skipped for ft-scoped specs because they install per-buffer at FileType
    -- time (the mapping isn't global yet at registration), and for keys with
    -- rhs == nil (keymap is owned by the plugin's own setup, no install on
    -- our side). False-positive guards: skip when the existing mapping was
    -- installed by core.pack itself — either via installed_global (real
    -- mappings from set_real_keymap) or via the desc prefix used by both
    -- set_real_keymap ("key: ") and set_stub_keymap ("lazy: ").
    if ft then return end
    local ext_key = mode .. ":" .. lhs
    if warned_external[ext_key] then return end
    if installed_global[ext_key] == spec.name then return end

    -- Validate collision opts. Bad combinations get one warning and the
    -- offending field is dropped; processing continues.
    local k_local = k and vim.deepcopy(k) or nil
    if k_local and k_local.preserve ~= nil and type(k_local.preserve) ~= "string" then
      vim.notify(
        ("core.pack: invalid preserve on '%s' (mode=%s); expected string lhs"):format(lhs, mode),
        vim.log.levels.WARN)
      k_local.preserve = nil
    end
    if k_local and k_local.override ~= nil and type(k_local.override) ~= "boolean" then
      vim.notify(
        ("core.pack: invalid override on '%s' (mode=%s); expected boolean"):format(lhs, mode),
        vim.log.levels.WARN)
      k_local.override = nil
    end
    if k_local and k_local.preserve and k_local.override then
      vim.notify(
        ("core.pack: preserve and override are mutually exclusive on '%s' (mode=%s); using preserve"):format(lhs, mode),
        vim.log.levels.WARN)
      k_local.override = nil
    end
    if k_local and k_local.preserve == lhs then
      vim.notify(
        ("core.pack: preserve target equals source on '%s' (mode=%s); ignoring"):format(lhs, mode),
        vim.log.levels.WARN)
      k_local.preserve = nil
    end

    local existing = vim.fn.maparg(lhs, mode, false, true)
    if not existing or vim.tbl_isempty(existing) then return end
    if existing.buffer ~= 0 then return end
    if existing.desc and (existing.desc:find("^key: ") or existing.desc:find("^lazy: ")) then return end
    warned_external[ext_key] = true  -- sentinel covers all branches below

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
    vim.notify(
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
      installed_global[m .. ":" .. lhs] = spec.name
    end
  end

  -- After load, warn if a string <Plug> rhs doesn't resolve to any mapping.
  -- Catches typos and plugin-renamed plugs.
  local function validate_plug(spec, k, m)
    local rhs = k[2]
    if type(rhs) ~= "string" or not rhs:match("^<Plug>") then return end
    if vim.fn.maparg(rhs, m) == "" then
      vim.notify(
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
      load_spec(spec, "key:" .. lhs)  -- install_spec_keys inside overrides this stub with the real mapping
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
      installed_global[m .. ":" .. lhs] = spec.name
    end
  end

  local function install_spec_stubs(spec)
    if not spec.keys then return end
    for _, k in ipairs(spec.keys) do
      for_each_mode(k, function(m) set_stub_keymap(spec, k, m) end)
    end
  end

  return {
    install_spec_keys  = install_spec_keys,
    install_spec_stubs = install_spec_stubs,
  }
end

return M
