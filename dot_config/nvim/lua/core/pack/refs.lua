-- core/pack/refs.lua
local M = {}

local Git     = require("core.pack.git")
local Version = require("core.pack.version")

-- Validate a string is safe to pass to git as a positional arg.
function M.validate(ref)
  if type(ref) ~= "string" or ref == "" then return false end
  if ref:sub(1, 1) == "-" then return false end
  return true
end

-- Compose a fully-qualified refspec. Used for human-readable display
-- and for passing to rev-parse — never used as a checkout argument
-- directly (checkout always uses the SHA from .resolve()).
function M.qualified(kind, name)
  if kind == "tag"    then return "refs/tags/" .. name end
  if kind == "branch" then return "refs/heads/" .. name end
  if kind == "commit" then return name end
  if kind == "default" then return "refs/remotes/origin/HEAD" end
  error("refs.qualified: unknown kind " .. tostring(kind))
end

-- Resolve a spec + post-clone working dir to a checkout-safe SHA.
-- Returns:
--   { kind = "tag"|"branch"|"commit"|"default", name = "...", sha = "..." }, nil
--   nil, "<error message>"  -- caller is expected to prefix with spec name
function M.resolve(spec, dir)
  local refs = Git.list_remote_refs(dir)
  refs.default_branch = Git.default_branch(dir)
  local resolved = Version.resolve(spec.version, refs)
  if not resolved then
    return nil, "could not resolve version (no matching tag/branch/sha)"
  end
  local qualified = M.qualified(resolved.kind, resolved.name)
  if not M.validate(qualified) then
    return nil, ("invalid ref %q (cannot start with -)"):format(qualified)
  end
  local sha, err = Git.rev_parse(dir, qualified .. "^{commit}")
  if not sha then
    return nil, ("ref '%s' did not resolve in %s: %s"):format(qualified, dir, err or "?")
  end
  return { kind = resolved.kind, name = resolved.name, sha = sha }
end

return M
