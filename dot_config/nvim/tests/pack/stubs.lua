-- tests/pack/stubs.lua
-- Stub helpers for unit tests.
local M = {}

-- Install a vim.system stub keyed by normalized command.
-- responses: map from command-key to { code = N, stdout = "...", stderr = "..." }
-- Command-key is the args joined by " " AFTER stripping leading "git" and
-- leading "-C <dir>". Examples:
--   git tag --list                       -> "tag --list"
--   git -C /some/dir rev-parse --verify HEAD -> "rev-parse --verify HEAD"
-- Supports both sync (`:wait()`) and async (`vim.system(cmd, opts, cb)`)
-- callers — production code now uses the latter exclusively.
-- Returns a `restore` closure; call it to reinstate the original vim.system.
function M.stub_system(responses)
  local orig = vim.system
  vim.system = function(cmd, _opts, callback)
    local args = vim.deepcopy(cmd)
    if args[1] == "git" then table.remove(args, 1) end
    if args[1] == "-C" then table.remove(args, 1); table.remove(args, 1) end
    local key = table.concat(args, " ")
    local response = responses[key]
    if not response then
      response = { code = 1, stderr = "stub_system: no response for: " .. key }
    end
    local result = {
      code   = response.code or 0,
      stdout = response.stdout or "",
      stderr = response.stderr or "",
    }
    if callback then
      -- Production code wraps the callback in vim.schedule; mirror that
      -- so tests pump the loop the same way the real code path does.
      vim.schedule(function() callback(result) end)
    end
    return {
      wait = function() return result end,
    }
  end
  return function() vim.system = orig end
end

-- Drive an async function (callback passed as last arg) to completion
-- and return whatever the callback received.
function M.await(fn, ...)
  local args = { ... }
  local n = select("#", ...)
  local results
  args[n + 1] = function(...) results = { n = select("#", ...), ... } end
  fn(unpack(args, 1, n + 1))
  vim.wait(5000, function() return results ~= nil end, 5)
  if not results then error("await: timed out waiting for callback") end
  return unpack(results, 1, results.n)
end

return M
