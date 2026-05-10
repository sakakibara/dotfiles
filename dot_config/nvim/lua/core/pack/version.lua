local M = {}

local function is_sha(s) return type(s) == "string" and #s >= 7 and s:match("^%x+$") ~= nil end

-- Pick the highest-semver tag from a refs.tags list, or nil.
local function highest_semver(tags)
  local best
  for _, tag in ipairs(tags or {}) do
    local v = vim.version.parse(tag, { strict = false })
    if v then
      if not best or vim.version.gt(v, best.v) then best = { tag = tag, v = v } end
    end
  end
  return best and best.tag or nil
end

function M.resolve(version, refs)
  if version == nil then
    return { kind = "default", name = nil }
  end

  -- Channel sentinels: "stable" tracks the highest semver tag, "edge"
  -- follows the default branch, "pinned" defers to the lockfile (the
  -- pin happens upstream in update via target = "lockfile"; here we
  -- just resolve to the default branch as a base, and the caller skips
  -- applying when the lockfile-pinned rev still matches).
  if version == "stable" then
    local tag = highest_semver(refs.tags)
    if tag then return { kind = "tag", name = tag } end
    return { kind = "default", name = nil }
  end
  if version == "edge" then
    return { kind = "default", name = nil }
  end
  if version == "pinned" then
    return { kind = "pinned", name = nil }
  end

  -- vim.version range
  if type(version) == "table" and version.has then
    local best
    for _, tag in ipairs(refs.tags or {}) do
      local v = vim.version.parse(tag, { strict = false })
      if v and version:has(v) then
        if not best or vim.version.gt(v, best.v) then best = { tag = tag, v = v } end
      end
    end
    if best then return { kind = "tag", name = best.tag } end
    return nil
  end

  if type(version) ~= "string" then return nil end

  -- Exact tag match takes precedence over branch (mirrors lazy.nvim/vim.pack).
  if vim.tbl_contains(refs.tags or {}, version) then return { kind = "tag", name = version } end
  if vim.tbl_contains(refs.branches or {}, version) then return { kind = "branch", name = version } end
  if is_sha(version) then return { kind = "commit", name = version } end
  return nil
end

return M
