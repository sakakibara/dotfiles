local M = {}
local uv = vim.uv or vim.loop

local TIMEOUT_MS = 10 * 60 * 1000
local POLL_MS = 25

local function safe_call(label, fn, ...)
  local ok, err = pcall(fn, ...)
  if not ok then
    vim.notify("core.pack.jobs: " .. label .. ": " .. tostring(err), vim.log.levels.ERROR)
  end
end

local function default_concurrency()
  return 2 * (uv.available_parallelism and uv.available_parallelism() or 2)
end

local Pool = {}
Pool.__index = Pool

function M.pool(opts)
  opts = opts or {}
  return setmetatable({
    concurrency = opts.concurrency or default_concurrency(),
    queue = {},
    inflight = 0,
    done = 0,
  }, Pool)
end

function Pool:add(job)
  -- job: { cmd, cwd?, on_start?, on_done?, tag? }
  table.insert(self.queue, job)
end

local function start_job(self, job, on_progress, total, on_all_done)
  if job.on_start then safe_call("on_start", job.on_start, job) end
  self.inflight = self.inflight + 1
  vim.system(job.cmd, { cwd = job.cwd, text = true }, function(result)
    -- vim.system callback runs in libuv thread; schedule for main loop.
    vim.schedule(function()
      result.tag = job.tag
      local released = false
      local function release()
        if released then return end
        released = true
        self.inflight = self.inflight - 1
        self.done = self.done + 1
        if on_progress then safe_call("on_progress", on_progress, self.done, total, result) end
        if #self.queue > 0 then
          local next_job = table.remove(self.queue, 1)
          start_job(self, next_job, on_progress, total, on_all_done)
        elseif self.inflight == 0 then
          on_all_done()
        end
      end
      if job.on_done then safe_call("on_done", job.on_done, result, release) end
      -- Default behavior: release the slot as soon as on_done returns.
      -- async_done = true keeps the slot held until on_done's chained
      -- work explicitly calls `release` (its 2nd arg), so concurrency
      -- caps cover the full async pipeline, not just the subprocess.
      if not job.async_done then release() end
    end)
  end)
end

-- Non-blocking. Returns immediately. on_complete fires once when all jobs settle.
function Pool:run(opts)
  opts = opts or {}
  local total = #self.queue
  self.done = 0  -- reset progress counter so on_progress reports correctly on re-runs
  local function on_all_done()
    if opts.on_complete then safe_call("on_complete", opts.on_complete) end
  end
  if total == 0 then
    -- Schedule so on_complete still fires asynchronously (consistent with the non-empty path).
    vim.schedule(on_all_done)
    return
  end
  local starting = math.min(self.concurrency, #self.queue)
  for _ = 1, starting do
    local job = table.remove(self.queue, 1)
    start_job(self, job, opts.on_progress, total, on_all_done)
  end
end

-- Test-only synchronous wrapper. Production code uses pool:run directly with on_complete.
function M.run_sync(pool, opts)
  opts = opts or {}
  local user_complete = opts.on_complete
  local done = false
  opts.on_complete = function()
    done = true
    if user_complete then user_complete() end
  end
  pool:run(opts)
  local ok = vim.wait(TIMEOUT_MS, function() return done end, POLL_MS)
  if not ok then
    vim.notify("core.pack.jobs: run_sync timed out after 10m", vim.log.levels.WARN)
  end
end

-- Coroutine-based async/await. `async(body)` returns a function that, when
-- invoked, starts a coroutine running `body(...)`. Inside the body, call
-- `await(fn, ...)` to suspend until `fn(..., cb)` invokes its callback;
-- `await` returns whatever `cb` was called with.
--
-- If the wrapped function is invoked with a final callback argument, that
-- callback receives the body's return values when the body finishes (or
-- `(nil, err)` if it raised).
function M.async(body)
  return function(...)
    local args = { n = select("#", ...), ... }
    local final_cb
    if args.n > 0 and type(args[args.n]) == "function" then
      final_cb = args[args.n]
      args[args.n] = nil
      args.n = args.n - 1
    end
    local co = coroutine.create(function()
      return body(unpack(args, 1, args.n))
    end)
    local function pack(...) return { n = select("#", ...), ... } end
    local function step(...)
      -- Hand-rolled pack preserves nils in the middle of return values;
      -- plain `{...}` plus `#` would mis-count once the boundary walks
      -- past a hole.
      local r = pack(coroutine.resume(co, ...))
      if not r[1] then
        vim.notify("core.pack.jobs.async: " .. tostring(r[2]), vim.log.levels.ERROR)
        if final_cb then final_cb(nil, r[2]) end
        return
      end
      if coroutine.status(co) == "dead" then
        if final_cb then final_cb(unpack(r, 2, r.n)) end
        return
      end
      -- Suspended: r[2] is the thunk yielded by `await`.
      r[2](step)
    end
    step()
  end
end

function M.await(fn, ...)
  local args = { n = select("#", ...), ... }
  return coroutine.yield(function(resume)
    args[args.n + 1] = function(...) resume(...) end
    fn(unpack(args, 1, args.n + 1))
  end)
end

-- Adapter to drain a pool from inside an async body:
--   await(Jobs.await_pool, pool, { on_progress = ... })
function M.await_pool(pool, opts, cb)
  opts = opts or {}
  local user_complete = opts.on_complete
  opts.on_complete = function()
    if user_complete then user_complete() end
    cb()
  end
  pool:run(opts)
end

return M
