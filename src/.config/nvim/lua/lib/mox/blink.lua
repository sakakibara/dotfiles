local M = {}
M.__index = M

local DIRECTIVES = {
  "when", "end", "include", "secret", "replace", "append", "prepend",
  "remove", "from", "for",
}

local AXES = { "os", "arch", "profile", "machine", "tool", "env", "path" }

local NAMESPACES = { "machine.", "env.", "data.", "entry.", "secret:" }

local MACHINE_FIELDS = {
  "os", "arch", "home", "hostname", "brew_prefix", "xdg_config_home", "tool_path.",
}

local _facts_cache = { mtime = 0, keys = {} }

function M.read_facts(path)
  local stat = vim.uv.fs_stat(path)
  if not stat then return {} end
  if stat.mtime.sec == _facts_cache.mtime then return _facts_cache.keys end
  local keys = {}
  local f = io.open(path, "r")
  if not f then return {} end
  for line in f:lines() do
    local key = line:match("^%s*([%w_]+)%s*=")
    if key then table.insert(keys, key) end
  end
  f:close()
  table.sort(keys)
  _facts_cache = { mtime = stat.mtime.sec, keys = keys }
  return keys
end

function M.directive_candidates(line_to_cursor)
  if not line_to_cursor:match("mox:%s*[%w]*$") then return nil end
  return DIRECTIVES
end

function M.axis_candidates(line_to_cursor, facts)
  if not line_to_cursor:match("mox:.*when%s") then return nil end
  if not line_to_cursor:match("[%s%(]n?o?t?%s*[%w_]*$") then return nil end
  local out = {}
  vim.list_extend(out, AXES)
  vim.list_extend(out, facts or {})
  return out
end

function M.capture_candidates(line_to_cursor, facts)
  local inner = line_to_cursor:match("<([%w_%.:]*)$")
  if not inner then return nil end
  local machine_prefix = inner:match("^machine%.([%w_%.]*)$")
  if machine_prefix then
    local out = {}
    vim.list_extend(out, MACHINE_FIELDS)
    vim.list_extend(out, facts or {})
    return out, "machine."
  end
  if inner:match("%.") or inner:match(":") then return nil end
  return NAMESPACES, ""
end

function M.new()
  return setmetatable({}, M)
end

function M:enabled()
  return vim.b.mox_source == true
end

function M:get_trigger_characters()
  return { "<", ".", " ", ":" }
end

function M:get_completions(_ctx, callback)
  local Kind = require("blink.cmp.types").CompletionItemKind
  local row_col = vim.api.nvim_win_get_cursor(0)
  local line = vim.api.nvim_get_current_line():sub(1, row_col[2])
  local facts = M.read_facts(vim.fn.expand("~/.config/mox/facts.toml"))
  local items = {}

  local directives = M.directive_candidates(line)
  if directives then
    for _, kw in ipairs(directives) do
      table.insert(items, { label = kw, kind = Kind.Keyword })
    end
  end

  local axes = M.axis_candidates(line, facts)
  if axes then
    for _, name in ipairs(axes) do
      table.insert(items, { label = name, kind = Kind.Variable })
    end
  end

  local captures, prefix = M.capture_candidates(line, facts)
  if captures then
    for _, c in ipairs(captures) do
      table.insert(items, { label = prefix .. c, kind = Kind.Field, insertText = c })
    end
  end

  callback({
    items = items,
    is_incomplete_backward = true,
    is_incomplete_forward = true,
  })
  return function() end
end

return M
