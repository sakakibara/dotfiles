-- lua/lib/colors/detect.lua
-- Hybrid detector: TS-first dispatcher (excludes comments etc.) with a
-- line-bounded regex fallback for filetypes whose parser isn't available.
-- Hard caps prevent runaway scans on huge buffers / single multi-MB lines.
local P = require("lib.colors.parse")
local M = {}

M.MAX_LINE_LEN    = 1000
M.MAX_VIEWPORT_KB = 256

-- TS queries indexed by filetype. Each query produces @color-candidate
-- captures whose text is run through P.parse_all.
M._queries = {
  css = [[
    ; CSS color values and call expressions. Comments are sibling nodes and
    ; are not captured here, so colors written in /* ... */ are correctly
    ; excluded.
    (color_value) @color-candidate
    (call_expression) @color-candidate
  ]],
}

-- Per-ft "tried" flag so we don't repeatedly attempt to load a missing parser.
M._ts_disabled = {}

local function ts_detect(buf, top, bot)
  local ft = vim.bo[buf].filetype
  if M._ts_disabled[ft] then return nil end
  local query_str = M._queries[ft]
  if not query_str then return nil end

  local ok, parser = pcall(vim.treesitter.get_parser, buf, ft)
  if not ok or not parser then
    M._ts_disabled[ft] = true
    return nil
  end

  local q
  ok, q = pcall(vim.treesitter.query.parse, ft, query_str)
  if not ok then
    vim.notify_once(
      "lib.colors: TS query failed for ft=" .. ft .. "; falling back to regex",
      vim.log.levels.WARN
    )
    M._ts_disabled[ft] = true
    return nil
  end

  local out = {}
  local trees = parser:parse({ top, bot + 1 })
  for _, tree in ipairs(trees) do
    for _, node in q:iter_captures(tree:root(), buf, top, bot + 1) do
      local sl, sc, el, ec = node:range()
      if sl >= top and sl <= bot and (el == sl or el == sl + 1) then
        local lines = vim.api.nvim_buf_get_text(buf, sl, sc, el, ec, {})
        local text = table.concat(lines, "\n")
        if #text <= M.MAX_LINE_LEN then
          for _, r in ipairs(P.parse_all(text)) do
            table.insert(out, {
              lnum  = sl,
              col_s = sc + r.range.col_s,
              col_e = sc + r.range.col_e,
              color = r.color,
            })
          end
        end
      end
    end
  end
  return out
end

local function regex_detect(buf, top, bot)
  local lines = vim.api.nvim_buf_get_lines(buf, top, bot + 1, false)
  local out = {}
  for i, line in ipairs(lines) do
    if #line <= M.MAX_LINE_LEN then
      for _, r in ipairs(P.parse_all(line)) do
        table.insert(out, {
          lnum  = top + i - 1,
          col_s = r.range.col_s,
          col_e = r.range.col_e,
          color = r.color,
        })
      end
    end
  end
  return out
end

function M.detect(buf, top, bot)
  if not vim.api.nvim_buf_is_valid(buf) then return {} end

  local lines = vim.api.nvim_buf_get_lines(buf, top, bot + 1, false)
  local total = 0
  for _, l in ipairs(lines) do total = total + #l end
  if total > M.MAX_VIEWPORT_KB * 1024 then return {} end

  return ts_detect(buf, top, bot) or regex_detect(buf, top, bot)
end

return M
