local M = {}

M._path_override = nil  -- tests

local function default_path()
  return vim.fn.stdpath("state") .. "/core-pack-log.jsonl"
end

function M.path()
  return M._path_override or default_path()
end

-- entry: { ts, kind = "update"|"install", name, from?, to?, count?, subject? }
function M.append(entry)
  local p = M.path()
  vim.fn.mkdir(vim.fn.fnamemodify(p, ":h"), "p")
  local fd = io.open(p, "a")
  if not fd then return end
  fd:write(vim.json.encode(entry) .. "\n")
  fd:close()
end

-- opts: { limit = N }
-- Returns newest-first list of entries.
function M.list(opts)
  opts = opts or {}
  local p = M.path()
  if vim.fn.filereadable(p) ~= 1 then return {} end
  local entries = {}
  for line in io.lines(p) do
    if line ~= "" then
      local ok, e = pcall(vim.json.decode, line)
      if ok and type(e) == "table" then entries[#entries + 1] = e end
    end
  end
  -- Reverse for newest-first.
  local out = {}
  for i = #entries, 1, -1 do out[#out + 1] = entries[i] end
  if opts.limit and #out > opts.limit then
    local trimmed = {}
    for i = 1, opts.limit do trimmed[i] = out[i] end
    return trimmed
  end
  return out
end

return M
