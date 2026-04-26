local T = require("tests.helpers")
local P = require("lib.colors.parse")

T.describe("lib.colors.parse hex", function()
  T.it("parse identifies hex literal at offset", function()
    local r = P.parse("color: #ff8800;", 8)
    T.truthy(r, "expected a result")
    T.eq(r.range.col_s, 7)   -- 0-indexed start of "#ff8800"
    T.eq(r.range.col_e, 14)  -- exclusive end
    T.eq(r.color.source.fmt, "hex")
  end)

  T.it("parse_all finds multiple hex literals on a line", function()
    local results = P.parse_all("a: #f00; b: #00ff00ff; c: #fff;")
    T.eq(#results, 3)
    T.eq(results[1].color.source.fmt, "hex")
    T.eq(results[2].color.source.fmt, "hex")
    T.eq(results[3].color.source.fmt, "hex")
  end)

  T.it("parse_all returns empty for no colors", function()
    local results = P.parse_all("just plain text")
    T.eq(#results, 0)
  end)
end)
