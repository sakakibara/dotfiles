-- lua/lib/markdown.lua
-- Toggle GFM task-list checkboxes on markdown lines.
--
-- Detection strategy is treesitter-first: if the `markdown` parser is
-- available, we query `task_list_marker_unchecked` / `task_list_marker_checked`
-- nodes, which means every listy edge case the tree-sitter grammar already
-- handles (nested lists, block quotes, lazy continuation, loose vs tight
-- lists, ordered lists with `1.` or `1)`, task markers inside code blocks
-- being *not* task markers, etc.) works without us re-implementing CommonMark.
-- When the parser isn't available we fall back to a permissive regex that
-- accepts common shapes (`- [ ] x`, `* [x]`, `1. [ ] y`, `> - [ ] z`,
-- `> > - [ ] z`). Toggle is a pure state flip on the 3-char marker span:
-- `[ ]` ⇄ `[x]`. We never insert a new marker — this is toggle-only so a
-- fat-fingered invocation on a plain list item is a no-op, not a surprise
-- edit.

local M = {}

local function strip_blockquotes(line)
  -- Lua patterns don't support repetition of a group, so we iterate. Each
  -- iteration peels one `>` (plus the optional leading indent and trailing
  -- space the CommonMark spec allows between `>` and content).
  local pos, s = 0, line
  while true do
    local cap = s:match("^%s*>%s?")
    if not cap then break end
    pos = pos + #cap
    s = s:sub(#cap + 1)
  end
  return pos, s
end

local function find_marker_regex(line)
  local bq_off, rest = strip_blockquotes(line)
  local lm = rest:match("^(%s*[%-%*%+]%s+)")
          or rest:match("^(%s*%d+[%.%)]%s+)")
  if not lm then return nil end
  local marker_col = bq_off + #lm
  local after_lm = rest:sub(#lm + 1)
  -- CommonMark requires whitespace after `]`, but we also accept the marker
  -- at EOL so fresh lines like `- [ ]` toggle before any content is typed.
  local state = after_lm:match("^%[([ xX])%]%s")
             or after_lm:match("^%[([ xX])%]$")
  if not state then return nil end
  return marker_col, state
end

local ts_query_src = [[
  (task_list_marker_unchecked) @unchecked
  (task_list_marker_checked)   @checked
]]

local function find_marker_ts(bufnr, parser, row)
  local ok_q, query = pcall(vim.treesitter.query.parse, "markdown", ts_query_src)
  if not ok_q then return nil end
  local trees = parser:parse()
  if not trees or not trees[1] then return nil end
  local root = trees[1]:root()
  for id, node in query:iter_captures(root, bufnr, row, row + 1) do
    local srow, scol = node:range()
    if srow == row then
      local state = (query.captures[id] == "checked") and "x" or " "
      return scol, state
    end
  end
  return nil
end

local function find_marker(bufnr, row)
  local has_ts, parser = pcall(vim.treesitter.get_parser, bufnr, "markdown")
  if has_ts and parser then
    return find_marker_ts(bufnr, parser, row)
  end
  local line = vim.api.nvim_buf_get_lines(bufnr, row, row + 1, false)[1]
  if not line then return nil end
  return find_marker_regex(line)
end

local function toggle_one(bufnr, row)
  local col, state = find_marker(bufnr, row)
  if not col then return false end
  local new_marker = (state == " ") and "[x]" or "[ ]"
  vim.api.nvim_buf_set_text(bufnr, row, col, row, col + 3, { new_marker })
  return true
end

-- Rows are 0-indexed, inclusive on both ends.
function M.toggle_range(bufnr, from_row, to_row)
  if bufnr == 0 or bufnr == nil then bufnr = vim.api.nvim_get_current_buf() end
  if from_row > to_row then from_row, to_row = to_row, from_row end
  local toggled = 0
  for row = from_row, to_row do
    if toggle_one(bufnr, row) then toggled = toggled + 1 end
  end
  return toggled
end

function M.toggle_line(bufnr, row)
  return M.toggle_range(bufnr, row, row)
end

return M
