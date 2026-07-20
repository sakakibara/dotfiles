local T = require("tests.helpers")

T.describe("core.event", function()
  T.it("VeryLazy fires once after schedule", function()
    package.loaded["core.event"] = nil
    local ev = require("core.event")
    local hits = 0
    ev.on("VeryLazy", function() hits = hits + 1 end)
    ev.trigger("VeryLazy")
    ev.trigger("VeryLazy")  -- second trigger is a no-op for handler
    T.eq(hits, 1)
  end)

  T.it("LazyFile expands to buffer events", function()
    package.loaded["core.event"] = nil
    local ev = require("core.event")
    T.eq(ev.expand("LazyFile"), { "BufReadPost", "BufNewFile", "BufWritePre" })
    T.eq(ev.expand("BufReadPre"), { "BufReadPre" })
    T.eq(ev.expand({ "LazyFile", "InsertEnter" }),
         { "BufReadPost", "BufNewFile", "BufWritePre", "InsertEnter" })
  end)
end)
