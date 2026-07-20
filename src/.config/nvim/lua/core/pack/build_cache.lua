-- Per-plugin build cache. Skips redundant build hook execution when
-- the source SHA + build command are unchanged since the last successful
-- build. Storage: a private git ref `refs/core-pack/built/<cmd_hash>`
-- inside the plugin's own .git/. Setting that ref to HEAD records
-- "build completed at this commit, with this build command." On future
-- attempts, ref == HEAD means skip. The ref namespace is private so
-- `git tag -l` / `git branch` / origin pushes don't see it.

local M = {}

local Git  = require("core.pack.git")
local Jobs = require("core.pack.jobs")

local async, await = Jobs.async, Jobs.await

-- Hash the build command's identity. Strings hash by content; functions
-- hash by their definition source (file:line) so the same function in
-- the same place produces a stable hash across nvim launches, while a
-- function moved to a different file invalidates the cache.
local function repr(build)
  if type(build) == "string" then return build end
  if type(build) == "function" then
    local info = debug.getinfo(build, "S")
    return ("fn:%s:%d"):format(info.source or "?", info.linedefined or 0)
  end
  return type(build) .. ":?"
end

local function ref_for(build)
  return "refs/core-pack/built/" .. vim.fn.sha256(repr(build)):sub(1, 16)
end

-- Returns true when the cached build covers the current HEAD.
M.is_fresh = async(function(dir, build)
  if build == nil or build == "" then return true end
  local r = await(Git.run, { "rev-parse", "--verify", "--quiet", ref_for(build) }, dir)
  if not r.ok then return false end
  local cached = (r.stdout or ""):gsub("%s+", "")
  if cached == "" then return false end
  local head = await(Git.run, { "rev-parse", "HEAD" }, dir)
  if not head.ok then return false end
  return cached == (head.stdout or ""):gsub("%s+", "")
end)

-- Pin the current HEAD as "successfully built with this command."
M.mark_built = async(function(dir, build)
  if build == nil or build == "" then return end
  await(Git.run, { "update-ref", ref_for(build), "HEAD" }, dir)
end)

-- Drop any cached entry for this plugin (used by :Pack build to force
-- a rebuild). Walks the private namespace and deletes whatever's there;
-- there's typically only one ref per plugin.
M.invalidate = async(function(dir)
  local r = await(Git.run,
    { "for-each-ref", "--format=%(refname)", "refs/core-pack/built/" }, dir)
  if not r.ok then return end
  for line in (r.stdout or ""):gmatch("[^\n]+") do
    await(Git.run, { "update-ref", "-d", line }, dir)
  end
end)

return M
