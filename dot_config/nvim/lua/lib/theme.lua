-- lua/lib/theme.lua
-- Reads $XDG_STATE_HOME/dotfiles/theme (single line "family/variant"),
-- written by the `theme` switcher. Falls back to catppuccin/mocha if the
-- state file is missing or malformed.

local M = {}

local DEFAULT = { family = "catppuccin", variant = "mocha" }

function M.path()
  local state = vim.env.XDG_STATE_HOME or (vim.env.HOME .. "/.local/state")
  return state .. "/dotfiles/theme"
end

function M.read()
  local ok, lines = pcall(vim.fn.readfile, M.path())
  if not ok or type(lines) ~= "table" or #lines == 0 then
    return vim.deepcopy(DEFAULT)
  end
  local family, variant = (lines[1] or ""):match("^([^/]+)/(.+)$")
  if not family or not variant then
    return vim.deepcopy(DEFAULT)
  end
  return { family = family, variant = variant }
end

return M
