local T = require("tests.helpers")

local U = require("lib.unicode")

T.describe("lib.unicode.width", function()
  T.it("counts display columns, not bytes", function()
    T.eq(U.width("abc"), 3)
    T.eq(U.width("日本語"), 6)
    T.eq(U.width("a日b"), 4)
    T.eq(U.width(""), 0)
  end)
end)

T.describe("lib.unicode.head", function()
  T.it("returns the whole string when it already fits", function()
    T.eq(U.head("hello", 10), "hello")
    T.eq(U.head("hello", 5), "hello")
  end)

  T.it("truncates ASCII at the column budget", function()
    T.eq(U.head("hello", 3), "hel")
  end)

  T.it("keeps a wide character whole rather than splitting it", function()
    T.eq(U.head("日本語", 3), "日")
    T.eq(U.head("日本語", 4), "日本")
  end)

  T.it("returns empty for a non-positive budget", function()
    T.eq(U.head("hello", 0), "")
    T.eq(U.head("hello", -1), "")
  end)
end)

T.describe("lib.unicode.tail", function()
  T.it("returns the whole string when it already fits", function()
    T.eq(U.tail("hello", 10), "hello")
  end)

  T.it("returns the widest suffix within the budget", function()
    T.eq(U.tail("hello", 3), "llo")
  end)

  T.it("keeps a wide character whole", function()
    T.eq(U.tail("日本語", 3), "語")
    T.eq(U.tail("日本語", 4), "本語")
  end)

  T.it("returns empty for a non-positive budget", function()
    T.eq(U.tail("hello", 0), "")
  end)
end)
