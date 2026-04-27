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
    pool:run({})
    T.eq(got.code, 0)
    T.truthy(got.stdout:match("hello"))
    T.eq(got.tag, "echo")
  end)

  T.it("respects concurrency cap", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 2 })
    -- Track peak concurrency by counting via a shared stdout marker.
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
    pool:run({})
    T.truthy(peak <= 2, "peak concurrency exceeded cap: " .. peak)
  end)

  T.it("emits on_progress per finished job with done/total counts", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 2 })
    for i = 1, 3 do pool:add({ cmd = { "true" }, tag = tostring(i) }) end
    local progress_seen = {}
    pool:run({ on_progress = function(done, total) progress_seen[#progress_seen + 1] = { done, total } end })
    T.eq(progress_seen[#progress_seen][1], 3)
    T.eq(progress_seen[#progress_seen][2], 3)
  end)

  T.it("collects per-job failures without aborting siblings", function()
    local J = fresh()
    local pool = J.pool({ concurrency = 2 })
    local results = {}
    pool:add({ cmd = { "false" },        tag = "a", on_done = function(r) results.a = r end })
    pool:add({ cmd = { "echo", "ok" },   tag = "b", on_done = function(r) results.b = r end })
    pool:run({})
    T.truthy(results.a.code ~= 0)
    T.eq(results.b.code, 0)
  end)
end)
