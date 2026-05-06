-- lua/lib/fold.lua
-- Custom `foldtext` for the native (vim.treesitter.foldexpr) fold engine.
-- Renders the fold's first line trimmed of leading whitespace, followed
-- by a "... N lines (P%)" suffix. One Folded highlight applies to the
-- whole line - native foldtext doesn't support per-segment highlights
-- (that would need extmark virt_text, which the foldtext option can't
-- emit).
--
-- Wired in lua/config/options.lua:
--     vim.opt.foldtext = "v:lua.Lib.fold.foldtext()"

local M = {}

function M.foldtext()
  if vim.bo.filetype == "org" then
    local ok, organ_fold = pcall(require, "organ.fold")
    if ok and organ_fold.foldtext then
      return organ_fold.foldtext()
    end
  end

  local start = vim.fn.getline(vim.v.foldstart)
  local folded = vim.v.foldend - vim.v.foldstart + 1
  local total = vim.api.nvim_buf_line_count(0)
  local pct = (total > 0) and math.floor(folded / total * 100) or 0

  -- Strip fold-marker syntax leftovers ({{{ and {{{N) and leading
  -- whitespace so the rendered line starts where the content does.
  local clean = start
    :gsub("{{{%d*", "")
    :gsub("^%s+", "")
    :gsub("%s+$", "")

  return ("%s %s %d lines (%d%%)"):format(
    clean,
    Lib.icons.status.Ellipsis,
    folded,
    pct
  )
end

return M
