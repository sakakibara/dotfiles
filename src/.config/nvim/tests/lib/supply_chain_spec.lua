local T = require("tests.helpers")
local S = require("lib.supply_chain")

local function policy(body)
  local path = vim.fn.tempname()
  if body then vim.fn.writefile({ body }, path) end
  return path
end

T.describe("lib.supply_chain", function()
  T.it("reads the release age in minutes as days, as a number or a numeric string", function()
    T.eq(S.release_min_age_days(policy('{ "release_min_age_minutes": 10080 }')), 7)
    T.eq(S.release_min_age_days(policy('{ "release_min_age_minutes": "10080" }')), 7)
    T.eq(S.release_min_age_days(policy('{ "release_min_age_minutes": 0 }')), 0)
  end)

  T.it("answers nil for a missing, unrendered or invalid policy, never a default", function()
    T.eq(S.release_min_age_days(policy(nil)), nil)
    T.eq(S.release_min_age_days(policy('{ "release_min_age_minutes": "<data.supply_chain.release_min_age_minutes>" }')), nil)
    T.eq(S.release_min_age_days(policy("not json")), nil)
    T.eq(S.release_min_age_days(policy('{ "release_min_age_minutes": -1 }')), nil)
    T.eq(S.release_min_age_days(policy('{ "release_min_age_minutes": 1.5 }')), nil)
    T.eq(S.release_min_age_days(policy("{}")), nil)
  end)
end)
