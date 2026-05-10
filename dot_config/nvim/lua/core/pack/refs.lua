-- core/pack/refs.lua
local M = {}

local Git     = require("core.pack.git")
local Jobs    = require("core.pack.jobs")
local Version = require("core.pack.version")

local async, await = Jobs.async, Jobs.await

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
  -- Post-clone with --filter=blob:none, branches live at
  -- refs/remotes/origin/<name>; refs/heads/<name> doesn't exist until
  -- a local tracking branch is created. Pass the remote refspec so
  -- rev-parse succeeds.
  if kind == "branch" then return "refs/remotes/origin/" .. name end
  if kind == "commit" then return name end
  if kind == "default" then return "refs/remotes/origin/HEAD" end
  error("refs.qualified: unknown kind " .. tostring(kind))
end

-- Resolve a spec + post-clone working dir to a checkout-safe SHA.
-- Callback receives:
--   ({ kind = "tag"|"branch"|"commit"|"default", name = "...", sha = "..." })
--   (nil, "<error message>")  -- caller is expected to prefix with spec name
M.resolve = async(function(spec, dir)
  local refs = await(Git.list_remote_refs, dir)
  refs.default_branch = await(Git.default_branch, dir)
  local resolved = Version.resolve(spec.version, refs)
  if not resolved then
    return nil, "could not resolve version (no matching tag/branch/sha)"
  end
  local qualified = M.qualified(resolved.kind, resolved.name)
  if not M.validate(qualified) then
    return nil, ("invalid ref %q (cannot start with -)"):format(qualified)
  end
  local sha, err = await(Git.rev_parse, dir, qualified .. "^{commit}")
  if not sha then
    return nil, ("ref '%s' did not resolve in %s: %s"):format(qualified, dir, err or "?")
  end
  if not M.validate(sha) or not sha:match("^%x+$") then
    return nil, ("rev-parse returned invalid SHA: %s"):format(sha)
  end
  return { kind = resolved.kind, name = resolved.name, sha = sha }
end)

return M
