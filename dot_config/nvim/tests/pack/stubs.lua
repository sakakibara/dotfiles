-- tests/pack/stubs.lua
-- Stub helpers for unit tests.
local M = {}

-- Install a vim.system stub keyed by normalized command.
-- responses: map from command-key to { code = N, stdout = "...", stderr = "..." }
-- Command-key is the args joined by " " AFTER stripping leading "git" and
-- leading "-C <dir>". Examples:
--   git tag --list                       -> "tag --list"
--   git -C /some/dir rev-parse --verify HEAD -> "rev-parse --verify HEAD"
-- Returns a `restore` closure; call it to reinstate the original vim.system.
function M.stub_system(responses)
  local orig = vim.system
  vim.system = function(cmd, _opts)
    local args = vim.deepcopy(cmd)
    if args[1] == "git" then table.remove(args, 1) end
    if args[1] == "-C" then table.remove(args, 1); table.remove(args, 1) end
    local key = table.concat(args, " ")
    local response = responses[key]
    if not response then
      response = { code = 1, stderr = "stub_system: no response for: " .. key }
    end
    return {
      wait = function()
        return {
          code   = response.code or 0,
          stdout = response.stdout or "",
          stderr = response.stderr or "",
        }
      end,
    }
  end
  return function() vim.system = orig end
end

return M
