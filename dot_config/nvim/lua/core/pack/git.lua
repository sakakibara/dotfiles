local M = {}

local function git(args, cwd)
  local cmd = { "git" }
  if cwd then table.insert(cmd, "-C"); table.insert(cmd, cwd) end
  vim.list_extend(cmd, args)
  local r = vim.system(cmd, { text = true }):wait()
  return { ok = r.code == 0, code = r.code, stdout = r.stdout or "", stderr = r.stderr or "" }
end

function M.is_repo(dir)
  if not dir or dir == "" then return false end
  local r = git({ "rev-parse", "--is-inside-work-tree" }, dir)
  return r.ok and r.stdout:match("true") ~= nil
end

function M.current_rev(dir)
  local r = git({ "rev-parse", "HEAD" }, dir)
  if not r.ok then return nil end
  return (r.stdout:gsub("%s+", ""))
end

function M.clone(url, dest)
  vim.fn.mkdir(vim.fn.fnamemodify(dest, ":h"), "p")
  local r = git({ "clone", "--filter=blob:none", url, dest })
  if not r.ok then return { ok = false, err = r.stderr } end
  return { ok = true }
end

function M.fetch(dir)
  local r = git({ "fetch", "--tags", "--prune", "origin" }, dir)
  if not r.ok then return { ok = false, err = r.stderr } end
  return { ok = true }
end

function M.checkout_sha(dir, sha)
  -- SHA is hex; cannot be misinterpreted as a flag. Detached so we don't
  -- accumulate local branches.
  local r = git({ "checkout", "--detach", "--quiet", sha }, dir)
  if not r.ok then return { ok = false, err = r.stderr } end
  return { ok = true }
end

function M.rev_parse(dir, ref)
  -- Resolve `ref` to a SHA. Pass `ref^{commit}` to peel annotated tags.
  -- Returns sha string on success, or nil + stderr on failure.
  local r = git({ "rev-parse", "--verify", ref }, dir)
  if not r.ok then return nil, r.stderr end
  local sha = (r.stdout:gsub("%s+", ""))
  if not sha:match("^%x+$") or #sha < 7 then
    return nil, "rev-parse returned non-SHA: " .. sha
  end
  return sha
end

function M.log_between(dir, a, b)
  local r = git({ "log", "--pretty=%H%x09%an%x09%ar%x09%s", a .. ".." .. b }, dir)
  if not r.ok then return nil end
  local out = {}
  for line in r.stdout:gmatch("[^\n]+") do
    local sha, author, ago, subj = line:match("^(%w+)\t([^\t]*)\t([^\t]*)\t(.*)$")
    if sha then out[#out + 1] = { sha = sha, author = author, ago = ago, subject = subj } end
  end
  return out
end

function M.list_remote_refs(dir)
  local refs = { tags = {}, branches = {} }
  local r1 = git({ "tag", "--list" }, dir)
  if r1.ok then
    for tag in r1.stdout:gmatch("[^\n]+") do refs.tags[#refs.tags + 1] = tag end
  end
  local r2 = git({ "branch", "-r", "--format=%(refname:short)" }, dir)
  if r2.ok then
    for line in r2.stdout:gmatch("[^\n]+") do
      local b = line:match("^origin/(.+)$")
      if b and b ~= "HEAD" then refs.branches[#refs.branches + 1] = b end
    end
  end
  return refs
end

function M.default_branch(dir)
  local r = git({ "symbolic-ref", "refs/remotes/origin/HEAD" }, dir)
  if r.ok then
    local b = r.stdout:match("refs/remotes/origin/(.+)")
    if b then return (b:gsub("%s+$", "")) end
  end
  -- Fallback: try common names.
  local refs = M.list_remote_refs(dir)
  for _, candidate in ipairs({ "main", "master" }) do
    if vim.tbl_contains(refs.branches, candidate) then return candidate end
  end
  return nil
end

return M
