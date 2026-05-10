-- Trigger inference from a freshly-cloned plugin's filesystem. When a
-- spec sets `auto = true` and declares no explicit triggers, core.pack
-- scans plugin/* (for user commands) and ftdetect/ftplugin/ (for
-- filetypes) to derive lazy-load triggers automatically.
--
-- Source files are parsed with the bundled treesitter grammars (lua,
-- vim) instead of regex so we correctly skip commands that appear inside
-- string literals or comments.

local M = {}

local function read_text(p)
  local fd = io.open(p, "r")
  if not fd then return "" end
  local content = fd:read("*a") or ""
  fd:close()
  return content
end

local LUA_QUERY = [[
  (function_call
    name: (_) @fn (#match? @fn "nvim_create_user_command")
    arguments: (arguments . (string content: (string_content) @cmd)))
]]

local VIM_QUERY = [[
  (command_statement (command_name) @cmd)
]]

local function captures_for(text, lang, query_str)
  local pok, parser = pcall(vim.treesitter.get_string_parser, text, lang)
  if not pok or not parser then return {} end
  local tree = parser:parse()[1]
  if not tree then return {} end
  local qok, query = pcall(vim.treesitter.query.parse, lang, query_str)
  if not qok or not query then return {} end
  local found = {}
  for id, node in query:iter_captures(tree:root(), text, 0, -1) do
    if query.captures[id] == "cmd" then
      local name = vim.treesitter.get_node_text(node, text)
      if name and name ~= "" then found[name] = true end
    end
  end
  return found
end

local function scan_commands(dir)
  local cmds = {}
  local plugin_dir = dir .. "/plugin"
  if vim.fn.isdirectory(plugin_dir) ~= 1 then return cmds end
  for _, entry in ipairs(vim.fn.readdir(plugin_dir) or {}) do
    local path = plugin_dir .. "/" .. entry
    local lang = entry:match("%.lua$") and "lua"
              or entry:match("%.vim$") and "vim"
    if lang then
      local query = lang == "lua" and LUA_QUERY or VIM_QUERY
      local text = read_text(path)
      for name in pairs(captures_for(text, lang, query)) do cmds[name] = true end
    end
  end
  return cmds
end

-- Filetype contributions live in ftdetect/<ft>.{lua,vim} (registers
-- detection autocmds) and ftplugin/<ft>.{lua,vim} (per-buffer setup).
-- Either is a strong signal that the plugin only matters for that ft.
local function scan_filetypes(dir)
  local fts = {}
  for _, sub in ipairs({ "ftdetect", "ftplugin" }) do
    local d = dir .. "/" .. sub
    if vim.fn.isdirectory(d) == 1 then
      for _, entry in ipairs(vim.fn.readdir(d) or {}) do
        local ft = entry:gsub("%.lua$", ""):gsub("%.vim$", "")
        if ft ~= entry and ft ~= "" then fts[ft] = true end
      end
    end
  end
  return fts
end

local function sorted_keys(t)
  local out = {}
  for k in pairs(t) do out[#out + 1] = k end
  table.sort(out)
  return out
end

function M.scan(dir)
  return {
    cmds = sorted_keys(scan_commands(dir)),
    fts  = sorted_keys(scan_filetypes(dir)),
  }
end

-- Apply inferred triggers to a spec in place. Only acts on specs where
-- `auto = true` AND no explicit cmd/ft/event/keys is set; otherwise the
-- explicit declaration always wins.
function M.apply(spec, dir)
  if not spec.auto then return false end
  if spec.event or spec.ft or spec.cmd or spec.keys then return false end
  local result = M.scan(dir)
  if #result.cmds == 0 and #result.fts == 0 then return false end
  if #result.cmds > 0 then spec.cmd = result.cmds end
  if #result.fts > 0  then spec.ft  = result.fts  end
  spec.lazy = true
  return true
end

return M
