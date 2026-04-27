local M = {}
local uv = vim.uv or vim.loop

local function default_concurrency()
  return 2 * (uv.available_parallelism and uv.available_parallelism() or 2)
end

local TIMEOUT_MS = 10 * 60 * 1000
local POLL_MS = 25

local function safe_call(label, fn, ...)
  local ok, err = pcall(fn, ...)
  if not ok then
    vim.notify("core.pack.jobs: " .. label .. ": " .. tostring(err), vim.log.levels.ERROR)
  end
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
      self.inflight = self.inflight - 1
      self.done = self.done + 1
      result.tag = job.tag
      if job.on_done then safe_call("on_done", job.on_done, result) end
      if on_progress then safe_call("on_progress", on_progress, self.done, total, result) end
      if #self.queue > 0 then
        local next_job = table.remove(self.queue, 1)
        start_job(self, next_job, on_progress, total, on_all_done)
      elseif self.inflight == 0 then
        on_all_done()
      end
    end)
  end)
end

function Pool:run(opts)
  opts = opts or {}
  local total = #self.queue
  local finished = (total == 0)
  local function on_all_done() finished = true end
  -- Start up to `concurrency` jobs.
  local starting = math.min(self.concurrency, #self.queue)
  for _ = 1, starting do
    local job = table.remove(self.queue, 1)
    start_job(self, job, opts.on_progress, total, on_all_done)
  end
  -- Block this coroutine until all settle.
  local ok = vim.wait(TIMEOUT_MS, function() return finished end, POLL_MS)
  if not ok then
    vim.notify("core.pack.jobs: pool timed out after 10m (some jobs may not have completed)",
      vim.log.levels.WARN)
  end
end

return M
