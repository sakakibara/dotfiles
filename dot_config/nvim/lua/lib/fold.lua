-- lua/lib/fold.lua
-- Custom 'foldtext' that returns {text, hl_group} tuples so the first
-- line of the fold is painted with treesitter syntax colors, followed by
-- a muted suffix. vim.treesitter.foldtext was removed in 0.11+, so we
-- walk captures ourselves.

local M = {}

-- Build a per-byte keep mask that drops {{{N markers plus the leading
-- and trailing whitespace they leave behind, while preserving original
-- byte indices so treesitter column lookups still align.
local function build_keep(line)
  local n = #line
  local keep = {}
  for k = 1, n do keep[k] = true end

  local i = 1
  while i <= n do
    local s, e = line:find("{{{%d*", i)
    if not s then break end
    for k = s, e do keep[k] = false end
    i = e + 1
  end

  for k = 1, n do
    local c = line:sub(k, k)
    if not keep[k] or c == " " or c == "\t" then
      keep[k] = false
    else
      break
    end
  end

  for k = n, 1, -1 do
    local c = line:sub(k, k)
    if not keep[k] or c == " " or c == "\t" then
      keep[k] = false
    else
      break
    end
  end

  return keep
end

-- Pick the most specific (innermost) non-private capture at this byte.
local function hl_at(bufnr, lnum, col)
  local caps = vim.treesitter.get_captures_at_pos(bufnr, lnum, col)
  for i = #caps, 1, -1 do
    local name = caps[i].capture
    if name and not name:find("^_") then
      return "@" .. name
    end
  end
  return "Folded"
end

local function push(segments, text, hl)
  local last = segments[#segments]
  if last and last[2] == hl then
    last[1] = last[1] .. text
  else
    segments[#segments + 1] = { text, hl }
  end
end

function M.foldtext()
  local bufnr = vim.api.nvim_get_current_buf()
  local lnum  = vim.v.foldstart - 1
  local line  = vim.api.nvim_buf_get_lines(bufnr, lnum, lnum + 1, false)[1] or ""
  local count = vim.v.foldend - vim.v.foldstart + 1
  local total = vim.api.nvim_buf_line_count(bufnr)
  local pct   = (total > 0) and math.floor(count / total * 100) or 0

  local segments = {}

  if #line > 0 then
    local keep = build_keep(line)
    local has_ts = pcall(vim.treesitter.get_parser, bufnr)
    if has_ts then
      for col = 1, #line do
        if keep[col] then
          push(segments, line:sub(col, col), hl_at(bufnr, lnum, col - 1))
        end
      end
    else
      local kept = {}
      for col = 1, #line do
        if keep[col] then kept[#kept + 1] = line:sub(col, col) end
      end
      if #kept > 0 then
        push(segments, table.concat(kept), "Folded")
      end
    end
  end

  local I = Lib.icons.status
  push(segments, "  " .. I.Ellipsis .. "  ", "Comment")
  push(segments, tostring(count),            "Number")
  push(segments, " lines ",                  "Comment")
  push(segments, ("(%d%%)"):format(pct),     "NonText")

  return segments
end

return M
