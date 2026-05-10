-- Hybrid detector: TS-first dispatcher (excludes comments etc.) with a
-- line-bounded regex fallback for filetypes whose parser isn't available.
-- Hard caps prevent runaway scans on huge buffers / single multi-MB lines.
local P = require("lib.colors.parse")
local M = {}

M.MAX_LINE_LEN    = 1000
M.MAX_VIEWPORT_KB = 256

-- Filetypes that may use the regex fallback when no TS query is available.
-- Keyed by filetype name (string → true). Default: empty (opt-in only).
M._regex_filetypes = {}

-- TS queries keyed by *parser language name* (not filetype). Each query
-- produces @color-candidate captures whose text is run through P.parse_all.
M._queries = {
  css = [[
    ; CSS color values and call expressions. Comments are sibling nodes and
    ; are not captured here, so colors written in /* ... */ are correctly
    ; excluded.
    (color_value) @color-candidate
    (call_expression) @color-candidate
  ]],

  -- JSX/TSX className attribute strings (and `class=`). String contents are
  -- captured verbatim; parse_all extracts Tailwind classes from those.
  -- Parser "javascript" handles javascriptreact ft; "tsx" handles typescriptreact.
  javascript = [[
    (jsx_attribute
      (property_identifier) @attr (#match? @attr "^class(Name)?$")
      (string (string_fragment) @color-candidate))
  ]],
  tsx = [[
    (jsx_attribute
      (property_identifier) @attr (#match? @attr "^class(Name)?$")
      (string (string_fragment) @color-candidate))
  ]],
  html = [[
    (attribute
      (attribute_name) @attr (#eq? @attr "class")
      (quoted_attribute_value (attribute_value) @color-candidate))
  ]],

  -- JSON: capture the inner text of every string. parse_all silently
  -- no-ops on non-color text so over-capture is fine; the cost is one
  -- regex run per string node.
  json = [[
    (string (string_content) @color-candidate)
  ]],

  -- YAML: scalar values come in three flavors. Patterns are independent;
  -- a parse failure on any one node type disables detection for the
  -- whole filetype, but these three are the standard tree-sitter-yaml
  -- node names since the 0.5+ grammar.
  yaml = [[
    (plain_scalar)         @color-candidate
    (double_quote_scalar)  @color-candidate
    (single_quote_scalar)  @color-candidate
  ]],
}

-- Filetypes whose treesitter parser language differs from the ft name.
-- Mirrors nvim-treesitter's registration (parsers.lua) so we don't depend on
-- the plugin being loaded at query-parse time.
M._ft_to_lang = {
  javascriptreact = "javascript",
  jsx             = "javascript",
  typescriptreact = "tsx",
}

-- Per-ft "tried" flag so we don't repeatedly attempt to load a missing parser.
M._ts_disabled = {}

local function ts_detect(buf, top, bot)
  local ft = vim.bo[buf].filetype
  if M._ts_disabled[ft] then return nil end
  -- Resolve filetype → parser language (e.g. "javascriptreact" → "javascript").
  local lang = M._ft_to_lang[ft] or vim.treesitter.language.get_lang(ft) or ft
  local query_str = M._queries[lang]
  if not query_str then return nil end

  local ok, parser = pcall(vim.treesitter.get_parser, buf, ft)
  if not ok or not parser then
    M._ts_disabled[ft] = true
    return nil
  end

  local q
  ok, q = pcall(vim.treesitter.query.parse, lang, query_str)
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

  local ts_result = ts_detect(buf, top, bot)
  if ts_result then return ts_result end

  -- TS unavailable. Regex fallback runs when either:
  --   (a) this ft has an authored TS query (parser unavailable in this env), OR
  --   (b) the ft is explicitly whitelisted in _regex_filetypes
  -- Without this gate the regex would scan arbitrary code files (C, Rust, Go,
  -- Lua…) and produce false positives on identifier substrings, URL fragments,
  -- hex-shaped tokens, etc.
  local ft = vim.bo[buf].filetype
  local lang = M._ft_to_lang[ft] or ft
  if not M._queries[lang] and not M._regex_filetypes[ft] then return {} end

  return regex_detect(buf, top, bot)
end

return M
