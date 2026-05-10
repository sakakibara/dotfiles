-- blink.cmp custom source for chezmoi templates. Active only in buffers
-- where lib.chezmoi has set b:chezmoi_template = true (so it doesn't
-- pollute completion in regular shell/lua/toml files).

local M = {}
M.__index = M

local KEYWORDS = {
  "range", "if", "with", "else", "end", "define", "template", "block",
}

-- chezmoi/sprig functions used in this repo plus a few common ones.
-- Add as needed.
local FUNCS = {
  "promptString",  "promptStringOnce",
  "promptBool",    "promptBoolOnce",
  "promptInt",     "promptIntOnce",
  "lookPath",      "joinPath",
  "include",       "fromToml",
  "quote",         "default",
  "has",           "index",
  "env",
  "eq", "ne", "or", "and", "not",
}

-- Built-in chezmoi data fields. Augmented at runtime by reading the user's
-- chezmoi.toml.
local BUILTINS = {
  ".chezmoi.os",
  ".chezmoi.arch",
  ".chezmoi.hostname",
  ".chezmoi.username",
  ".chezmoi.homeDir",
  ".chezmoi.sourceDir",
  ".chezmoi.workingTree",
  ".chezmoi.kernel",
  ".chezmoi.osRelease",
}

-- Cheap line-based parser for the user's chezmoi.toml. Picks up top-level
-- keys (the [data] section's top-level keys end up exposed as `.<key>` in
-- templates). Does not handle nested tables or arrays-of-tables — uncommon
-- for chezmoi data — those fall back to manual typing.
local _data_cache = { mtime = 0, fields = {} }
local function read_user_data_fields()
  local path = vim.fn.expand("~/.config/chezmoi/chezmoi.toml")
  local stat = vim.uv.fs_stat(path)
  if not stat then return {} end
  if stat.mtime.sec == _data_cache.mtime then return _data_cache.fields end

  local fields = {}
  local in_data = false
  for _, line in ipairs(vim.fn.readfile(path)) do
    local trimmed = line:gsub("^%s+", ""):gsub("%s+$", "")
    if trimmed:match("^%[data%]") then
      in_data = true
    elseif trimmed:match("^%[") then
      in_data = false
    elseif in_data then
      local key = trimmed:match("^([%w_]+)%s*=")
      if key then table.insert(fields, "." .. key) end
    end
  end
  _data_cache = { mtime = stat.mtime.sec, fields = fields }
  return fields
end

-- ---------- blink.cmp source interface ----------

function M.new()
  return setmetatable({}, M)
end

function M:enabled()
  return vim.b.chezmoi_template == true
end

function M:get_trigger_characters()
  return { ".", "{" }
end

function M:get_completions(_ctx, callback)
  local Kind = require("blink.cmp.types").CompletionItemKind
  local items = {}

  for _, kw in ipairs(KEYWORDS) do
    table.insert(items, { label = kw, kind = Kind.Keyword })
  end
  for _, fn in ipairs(FUNCS) do
    table.insert(items, { label = fn, kind = Kind.Function })
  end
  for _, f in ipairs(BUILTINS) do
    table.insert(items, { label = f, kind = Kind.Field })
  end
  for _, f in ipairs(read_user_data_fields()) do
    table.insert(items, { label = f, kind = Kind.Field })
  end

  callback({
    items = items,
    is_incomplete_backward = false,
    is_incomplete_forward = false,
  })
  return function() end  -- cancellation noop
end

return M
