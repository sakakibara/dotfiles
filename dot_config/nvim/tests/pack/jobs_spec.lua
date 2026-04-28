local T = require("tests.helpers")

local function fresh()
  package.loaded["core.pack.jobs"] = nil
  return require("core.pack.jobs")
end

T.describe("core.pack.jobs", function()
  T.it("runs a single command to completion", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 1 })
    local got
    pool:add({ cmd = { "echo", "hello" }, on_done = function(r) got = r end, tag = "echo" })
    J.run_sync(pool, {})
    T.eq(got.code, 0)
    T.truthy(got.stdout:match("hello"))
    T.eq(got.tag, "echo")
  end)

  T.it("respects concurrency cap", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 2 })
    local active = 0
    local peak = 0
    for i = 1, 6 do
      pool:add({
        cmd = { "sh", "-c", "sleep 0.05" },
        tag = tostring(i),
        on_start = function() active = active + 1; if active > peak then peak = active end end,
        on_done = function() active = active - 1 end,
      })
    end
    J.run_sync(pool, {})
    T.truthy(peak <= 2, "peak concurrency exceeded cap: " .. peak)
  end)

  T.it("emits on_progress per finished job with done/total counts", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 2 })
    for i = 1, 3 do pool:add({ cmd = { "true" }, tag = tostring(i) }) end
    local progress_seen = {}
    J.run_sync(pool, { on_progress = function(done, total) progress_seen[#progress_seen + 1] = { done, total } end })
    T.eq(progress_seen[#progress_seen][1], 3)
    T.eq(progress_seen[#progress_seen][2], 3)
  end)

  T.it("collects per-job failures without aborting siblings", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 2 })
    local results = {}
    pool:add({ cmd = { "false" },        tag = "a", on_done = function(r) results.a = r end })
    pool:add({ cmd = { "echo", "ok" },   tag = "b", on_done = function(r) results.b = r end })
    J.run_sync(pool, {})
    T.truthy(results.a.code ~= 0)
    T.eq(results.b.code, 0)
  end)

  T.it("pool:run returns immediately and fires on_complete asynchronously", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 1 })
    pool:add({ cmd = { "sh", "-c", "sleep 0.05" }, tag = "slow" })
    local completed = false
    pool:run({ on_complete = function() completed = true end })
    T.eq(completed, false)
    vim.wait(5000, function() return completed end, 10)
    T.eq(completed, true)
  end)

  T.it("empty queue calls on_complete immediately on schedule tick", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 1 })
    local completed = false
    pool:run({ on_complete = function() completed = true end })
    vim.wait(1000, function() return completed end, 10)
    T.eq(completed, true)
  end)
end)
