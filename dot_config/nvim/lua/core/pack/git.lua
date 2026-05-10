local M = {}

-- Run a git subcommand asynchronously. The callback receives a uniform
-- result table on the main loop (vim.system's on_exit fires off-thread).
local function run(args, cwd, cb)
  local cmd = { "git" }
  if cwd then table.insert(cmd, "-C"); table.insert(cmd, cwd) end
  vim.list_extend(cmd, args)
  vim.system(cmd, { text = true }, function(r)
    vim.schedule(function()
      cb({
        ok     = r.code == 0,
        code   = r.code,
        stdout = r.stdout or "",
        stderr = r.stderr or "",
      })
    end)
  end)
end

M.run = run

-- File-existence check; sync because it touches no subprocess. We always
-- clone full repos, so .git is a directory.
function M.is_repo(dir)
  if not dir or dir == "" then return false end
  return vim.fn.isdirectory(dir .. "/.git") == 1
end

function M.current_rev(dir, cb)
  run({ "rev-parse", "HEAD" }, dir, function(r)
    if not r.ok then return cb(nil) end
    cb((r.stdout:gsub("%s+", "")))
  end)
end

function M.clone(url, dest, cb)
  vim.fn.mkdir(vim.fn.fnamemodify(dest, ":h"), "p")
  run({ "clone", "--filter=blob:none", url, dest }, nil, function(r)
    if not r.ok then return cb({ ok = false, err = r.stderr }) end
    cb({ ok = true })
  end)
end

function M.fetch(dir, cb)
  run({ "fetch", "--tags", "--prune", "origin" }, dir, function(r)
    if not r.ok then return cb({ ok = false, err = r.stderr }) end
    cb({ ok = true })
  end)
end

function M.checkout_sha(dir, sha, cb)
  -- SHA is hex; cannot be misinterpreted as a flag. Detached so we don't
  -- accumulate local branches.
  run({ "checkout", "--detach", "--quiet", sha }, dir, function(r)
    if not r.ok then return cb({ ok = false, err = r.stderr }) end
    cb({ ok = true })
  end)
end

function M.rev_parse(dir, ref, cb)
  -- Resolve `ref` to a SHA. Pass `ref^{commit}` to peel annotated tags.
  -- Callback receives (sha) on success, (nil, err) on failure.
  run({ "rev-parse", "--verify", ref }, dir, function(r)
    if not r.ok then return cb(nil, r.stderr) end
    local sha = (r.stdout:gsub("%s+", ""))
    if not sha:match("^%x+$") or #sha < 7 then
      return cb(nil, "rev-parse returned non-SHA: " .. sha)
    end
    cb(sha)
  end)
end

function M.log_between(dir, a, b, cb)
  run({ "log", "--pretty=%H%x09%an%x09%ar%x09%s", a .. ".." .. b }, dir, function(r)
    if not r.ok then return cb(nil) end
    local out = {}
    for line in r.stdout:gmatch("[^\n]+") do
      local sha, author, ago, subj = line:match("^(%w+)\t([^\t]*)\t([^\t]*)\t(.*)$")
      if sha then out[#out + 1] = { sha = sha, author = author, ago = ago, subject = subj } end
    end
    cb(out)
  end)
end

function M.list_remote_refs(dir, cb)
  local refs = { tags = {}, branches = {} }
  local pending = 2
  local function done()
    pending = pending - 1
    if pending == 0 then cb(refs) end
  end
  run({ "tag", "--list" }, dir, function(r)
    if r.ok then
      for tag in r.stdout:gmatch("[^\n]+") do refs.tags[#refs.tags + 1] = tag end
    end
    done()
  end)
  run({ "branch", "-r", "--format=%(refname:short)" }, dir, function(r)
    if r.ok then
      for line in r.stdout:gmatch("[^\n]+") do
        local b = line:match("^origin/(.+)$")
        if b and b ~= "HEAD" then refs.branches[#refs.branches + 1] = b end
      end
    end
    done()
  end)
end

function M.default_branch(dir, cb)
  run({ "symbolic-ref", "refs/remotes/origin/HEAD" }, dir, function(r)
    if r.ok then
      local b = r.stdout:match("refs/remotes/origin/(.+)")
      if b then return cb((b:gsub("%s+$", ""))) end
    end
    M.list_remote_refs(dir, function(refs)
      for _, candidate in ipairs({ "main", "master" }) do
        if vim.tbl_contains(refs.branches, candidate) then return cb(candidate) end
      end
      cb(nil)
    end)
  end)
end

return M
