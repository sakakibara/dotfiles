local T = require("tests.helpers")

local Slug = require("lib.preview.slug")

T.describe("lib.preview.slug", function()
  T.it("lowercases and turns spaces into hyphens", function()
    T.eq(Slug.slug("Hello World"), "hello-world")
  end)

  T.it("drops punctuation but keeps hyphens and underscores", function()
    T.eq(Slug.slug("Hello, World!"), "hello-world")
    T.eq(Slug.slug("foo_bar-baz"), "foo_bar-baz")
    T.eq(Slug.slug("What's new?"), "whats-new")
    T.eq(Slug.slug("v1.2.3"), "v123")
  end)

  T.it("keeps one hyphen per space, so dropped words leave doubled hyphens", function()
    T.eq(Slug.slug("C++ & Rust"), "c--rust")
    T.eq(Slug.slug("a  b"), "a--b")
  end)

  T.it("keeps and lowercases non-ASCII letters", function()
    T.eq(Slug.slug("Caf\195\169 \195\156n\195\175code"), "caf\195\169-\195\188n\195\175code")
    T.eq(Slug.slug("\230\151\165\230\156\172\232\170\158"), "\230\151\165\230\156\172\232\170\158")
  end)

  T.it("drops CJK punctuation, typographic quotes and emoji", function()
    T.eq(Slug.slug("\230\151\165\230\156\172\227\128\129\227\131\134\227\130\185\227\131\136\227\128\130"),
      "\230\151\165\230\156\172\227\131\134\227\130\185\227\131\136")
    T.eq(Slug.slug("It\226\128\153s \226\128\148 done"), "its--done")
    T.eq(Slug.slug("Emoji \240\159\142\137 party"), "emoji--party")
  end)

  T.it("dedupes repeats with -1, -2 like GitHub", function()
    local s = Slug.new()
    T.eq(s("Usage"), "usage")
    T.eq(s("Usage"), "usage-1")
    T.eq(s("Usage"), "usage-2")
    T.eq(s("Usage 1"), "usage-1-1")
  end)
end)
