-- Reads $XDG_STATE_HOME/dotfiles/theme, written by the `theme` switcher.
-- The state file is one of:
--   "family/variant"  for variant'd families (catppuccin, tokyonight, ...)
--   "family"          for no-variant families (dracula, nord, ...)
-- Falls back to catppuccin/mocha if the file is missing or malformed.

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
  local line = (lines[1] or ""):gsub("^%s+", ""):gsub("%s+$", "")
  if line == "" then
    return vim.deepcopy(DEFAULT)
  end
  local slash = line:find("/", 1, true)
  if slash then
    return { family = line:sub(1, slash - 1), variant = line:sub(slash + 1) }
  end
  return { family = line, variant = "" }
end

return M
