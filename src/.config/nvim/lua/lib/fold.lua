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

local function line_highlights(parser, bufnr, row, len)
  local hls = {}
  parser:for_each_tree(function(tstree, tree)
    if not tstree then return end
    local root = tstree:root()
    local root_start, _, root_end = root:range()
    if root_start > row or root_end < row then return end
    local query = vim.treesitter.query.get(tree:lang(), "highlights")
    if not query then return end
    for id, node in query:iter_captures(root, bufnr, row, row + 1) do
      local name = query.captures[id]
      if name and not name:find("^_") then
        local sr, sc, er, ec = node:range()
        local first = sr < row and 0 or sc
        local last = er > row and len or math.min(ec, len)
        for col = first + 1, last do hls[col] = "@" .. name end
      end
    end
  end)
  return hls
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
    local parser = vim.treesitter.get_parser(bufnr, nil, { error = false })
    if parser then
      local hls = line_highlights(parser, bufnr, lnum, #line)
      for col = 1, #line do
        if keep[col] then
          push(segments, line:sub(col, col), hls[col] or "Folded")
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
