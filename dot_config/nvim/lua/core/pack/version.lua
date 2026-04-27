local M = {}

local function is_sha(s) return type(s) == "string" and #s >= 7 and s:match("^%x+$") ~= nil end

function M.resolve(version, refs)
  if version == nil then
    if refs.default_branch then return { kind = "branch", ref = refs.default_branch } end
    return nil
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
    if best then return { kind = "tag", ref = best.tag } end
    return nil
  end

  if type(version) ~= "string" then return nil end

  -- Exact tag match takes precedence over branch (mirrors lazy.nvim/vim.pack).
  if vim.tbl_contains(refs.tags or {}, version) then return { kind = "tag", ref = version } end
  if vim.tbl_contains(refs.branches or {}, version) then return { kind = "branch", ref = version } end
  if is_sha(version) then return { kind = "commit", ref = version } end
  return nil
end

return M
