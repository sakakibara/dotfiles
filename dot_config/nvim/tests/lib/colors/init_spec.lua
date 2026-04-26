local T = require("tests.helpers")

T.describe("lib.colors", function()
  T.it("registers under Lib.colors", function()
    package.loaded["lib.colors"] = nil
    require("lib").init()
    T.truthy(Lib.colors, "Lib.colors not registered")
    T.truthy(type(Lib.colors.setup) == "function", "Lib.colors.setup missing")
  end)
end)
