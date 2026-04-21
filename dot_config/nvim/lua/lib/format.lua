-- lua/lib/format.lua
--
-- Pluggable format-on-save with composable sources, vim.g / vim.b autoformat
-- toggles, and quiet-by-default on-save semantics.
--
-- Design:
--   - Formatter sources register via M.register({ name, primary, priority,
--     sources(buf), format(buf) }).
--   - M.resolve(buf) orders them by priority and decides which are `active`:
--     a source is active if it has sources>0 AND (it's non-primary OR no
--     higher-priority primary was already chosen).
--   - M.format({buf, force}) runs all active formatters. `force` bypasses the
--     autoformat-enabled check (used by :Format / <leader>cf).
--   - On-save calls M.format({buf}) without force: no-op if disabled, no-op
--     if no source has anything to do. No spam on every save.
--   - With force AND nothing active: warn (the user asked — they should
--     know why nothing happened).
--
-- Why vim.g/vim.b.autoformat specifically: standard convention (LazyVim,
-- AstroVim, etc.) so snippets/docs from those communities port over
-- unchanged. Also makes per-project ftplugin overrides trivial.

local M = setmetatable({}, {
  __call = function(m, ...) return m.format(...) end,
})

---@class FormatSource
---@field name string
---@field primary? boolean    only one primary runs at a time (highest priority wins)
---@field priority number     higher runs first; sort-stable
---@field format fun(buf:number)
---@field sources fun(buf:number):string[]   names of formatters/clients that would run

M.formatters = {} ---@type FormatSource[]

---@param src FormatSource
function M.register(src)
  M.formatters[#M.formatters + 1] = src
  table.sort(M.formatters, function(a, b) return (a.priority or 0) > (b.priority or 0) end)
end

-- Resolve which sources apply to a buffer right now. Returns a list of
-- wrapped records (the source table as __index, plus `active` and `resolved`).
function M.resolve(buf)
  buf = (buf == nil or buf == 0) and vim.api.nvim_get_current_buf() or buf
  local have_primary = false
  local ret = {}
  for _, f in ipairs(M.formatters) do
    local ok, sources = pcall(f.sources, buf)
    if not ok then sources = {} end
    local active = #sources > 0 and (not f.primary or not have_primary)
    if active and f.primary then have_primary = true end
    ret[#ret + 1] = setmetatable({
      active = active,
      resolved = sources,
    }, { __index = f })
  end
  return ret
end

-- Is autoformat-on-save enabled for this buffer? vim.b overrides vim.g.
-- Default: true.
function M.enabled(buf)
  buf = (buf == nil or buf == 0) and vim.api.nvim_get_current_buf() or buf
  local baf = vim.b[buf].autoformat
  if baf ~= nil then return baf end
  local gaf = vim.g.autoformat
  return gaf == nil or gaf
end

-- Set autoformat state. If `buf` is truthy, scope to the current buffer
-- (vim.b.autoformat). If falsy/nil, set global (vim.g.autoformat) and clear
-- the current buffer's override so the new global takes effect.
function M.enable(enable, buf)
  if enable == nil then enable = true end
  if buf then
    vim.b.autoformat = enable
  else
    vim.g.autoformat = enable
    vim.b.autoformat = nil
  end
  pcall(vim.cmd, "redrawstatus")
end

function M.toggle(buf)
  M.enable(not M.enabled(), buf)
end

-- Run all active formatters. opts.force bypasses the enabled check.
-- When force=true and nothing was formatted, warns.
function M.format(opts)
  opts = opts or {}
  local buf = opts.buf or vim.api.nvim_get_current_buf()
  if buf == 0 then buf = vim.api.nvim_get_current_buf() end
  if not (opts.force or M.enabled(buf)) then return end

  local done = false
  for _, f in ipairs(M.resolve(buf)) do
    if f.active then
      done = true
      local ok, err = pcall(function() return f.format(buf) end)
      if not ok then
        vim.notify(("Formatter %s failed: %s"):format(f.name, err), vim.log.levels.WARN)
      end
    end
  end

  if not done and opts.force then
    vim.notify("No formatters available for this buffer", vim.log.levels.WARN)
  end
end

-- Summarize current state + available sources. Writes markdown so
-- snacks.notifier (default ft=markdown) renders bold/inline-code/italic
-- as a proper floating card — not a drab :messages dump.
function M.info(buf)
  buf = (buf == nil or buf == 0) and vim.api.nvim_get_current_buf() or buf
  local gaf = vim.g.autoformat == nil or vim.g.autoformat
  local baf = vim.b[buf].autoformat
  local enabled = M.enabled(buf)

  local function code_list(xs)
    local out = {}
    for i, s in ipairs(xs) do out[i] = "`" .. s .. "`" end
    return table.concat(out, " ")
  end

  local lines = {
    ("**global**  `%s`"):format(gaf and "on" or "off"),
    ("**buffer**  `%s`"):format(baf == nil and "inherit" or (baf and "on" or "off")),
  }

  local rows = {}
  for _, f in ipairs(M.resolve(buf)) do
    if #f.resolved > 0 then rows[#rows + 1] = f end
  end

  if #rows == 0 then
    lines[#lines + 1] = ""
    lines[#lines + 1] = "_no formatters for this buffer_"
  else
    lines[#lines + 1] = ""
    for _, f in ipairs(rows) do
      if f.active then
        lines[#lines + 1] = ("▸ **%s**  %s"):format(f.name, code_list(f.resolved))
      else
        lines[#lines + 1] = ("· %s  %s  _(shadowed)_"):format(f.name, code_list(f.resolved))
      end
    end
  end

  vim.notify(
    table.concat(lines, "\n"),
    enabled and vim.log.levels.INFO or vim.log.levels.WARN,
    { title = "Autoformat " .. (enabled and "on" or "off") }
  )
end

-- Called from vim.opt.formatexpr via options.lua.
function M.formatexpr()
  local ok, conform = pcall(require, "conform")
  if ok then return conform.formatexpr() end
  return vim.lsp.formatexpr({ timeout_ms = 1000 })
end

function M.setup()
  -- Built-in LSP source — non-primary so it runs alongside a primary
  -- formatter (e.g. conform) when both have sources; takes over fully if
  -- no primary has anything configured.
  M.register({
    name = "LSP",
    primary = false,
    priority = 1,
    format = function(buf)
      vim.lsp.buf.format({ bufnr = buf, timeout_ms = 1000 })
    end,
    sources = function(buf)
      local names = {}
      for _, c in ipairs(vim.lsp.get_clients({ bufnr = buf, method = "textDocument/formatting" })) do
        names[#names + 1] = c.name
      end
      return names
    end,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    group = vim.api.nvim_create_augroup("Lib.format", { clear = true }),
    callback = function(args) M.format({ buf = args.buf }) end,
  })

  vim.api.nvim_create_user_command("Format", function(opts)
    M.format({ force = true, buf = opts.range > 0 and 0 or nil })
  end, { range = true, desc = "Format current buffer (or range)" })

  vim.api.nvim_create_user_command("FormatInfo", function() M.info() end,
    { desc = "Show autoformat state and available formatters" })

  vim.api.nvim_create_user_command("FormatToggle", function(opts)
    M.toggle(opts.bang and true or nil)
  end, { bang = true, desc = "Toggle autoformat (! scopes to current buffer)" })

  vim.api.nvim_create_user_command("FormatEnable", function(opts)
    M.enable(true, opts.bang and true or nil)
  end, { bang = true, desc = "Enable autoformat (! scopes to current buffer)" })

  vim.api.nvim_create_user_command("FormatDisable", function(opts)
    M.enable(false, opts.bang and true or nil)
  end, { bang = true, desc = "Disable autoformat (! scopes to current buffer)" })
end

return M
