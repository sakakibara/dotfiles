local T = require("tests.helpers")

local function fresh()
  package.loaded["lib.parsers.health"] = nil
  return require("lib.parsers.health")
end

local function queries(tree)
  local root = vim.fn.tempname()
  for lang, files in pairs(tree) do
    vim.fn.mkdir(root .. "/" .. lang, "p")
    for name, body in pairs(files) do
      vim.fn.writefile(vim.split(body, "\n"), root .. "/" .. lang .. "/" .. name .. ".scm")
    end
  end
  return function(lang) return root .. "/" .. lang end
end

local function deps(registry, requires, query_dir)
  return {
    fts = function()
      local out = vim.tbl_keys(registry)
      table.sort(out)
      return out
    end,
    parsers_for = function(ft) return registry[ft] or {} end,
    requires = function(lang) return (requires or {})[lang] or {} end,
    query_dir = query_dir,
  }
end

T.describe("lib.parsers.health inherit_gaps", function()
  T.it("reports a language a query inherits that no registered parser provides", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "; inherits: html" } })
    T.eq(
      H.inherit_gaps(deps({ svelte = { "svelte" } }, {}, q)),
      { { ft = "svelte", missing = { { lang = "html", via = "svelte" } } } }
    )
  end)

  T.it("reports nothing when the inherited language is registered for the filetype", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "; inherits: html" }, html = { highlights = "(x) @y" } })
    T.eq(H.inherit_gaps(deps({ svelte = { "svelte", "html" } }, {}, q)), {})
  end)

  T.it("treats a parser pulled in by requires as providing the inherited language", function()
    local H = fresh()
    local q = queries({ svelte = { injections = "; inherits: html_tags" }, html_tags = { highlights = "(x) @y" } })
    T.eq(H.inherit_gaps(deps({ svelte = { "svelte" } }, { svelte = { "html_tags" } }, q)), {})
  end)

  T.it("follows inheritance transitively through languages that are themselves missing", function()
    local H = fresh()
    local q = queries({
      svelte    = { highlights = "; inherits: html" },
      html      = { highlights = "; inherits: html_tags" },
      html_tags = { highlights = "(x) @y" },
    })
    T.eq(
      H.inherit_gaps(deps({ svelte = { "svelte" } }, {}, q)),
      { { ft = "svelte", missing = { { lang = "html", via = "svelte" }, { lang = "html_tags", via = "html" } } } }
    )
  end)

  T.it("ignores a query that inherits its own language", function()
    local H = fresh()
    local q = queries({ lua = { highlights = "; inherits: lua" } })
    T.eq(H.inherit_gaps(deps({ lua = { "lua" } }, {}, q)), {})
  end)

  T.it("does not report a language the missing language's own requires would provide", function()
    local H = fresh()
    local q = queries({
      svelte    = { highlights = "; inherits: html" },
      html      = { highlights = "; inherits: html_tags" },
      html_tags = { highlights = "(x) @y" },
    })
    T.eq(
      H.inherit_gaps(deps({ svelte = { "svelte" } }, { html = { "html_tags" } }, q)),
      { { ft = "svelte", missing = { { lang = "html", via = "svelte" } } } }
    )
  end)

  T.it("reads an inherits modeline below other leading comment lines", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "; a note\n; inherits: html" } })
    T.eq(
      H.inherit_gaps(deps({ svelte = { "svelte" } }, {}, q)),
      { { ft = "svelte", missing = { { lang = "html", via = "svelte" } } } }
    )
  end)

  T.it("stops scanning at the first line that is not a comment", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "(x) @y\n; inherits: html" } })
    T.eq(H.inherit_gaps(deps({ svelte = { "svelte" } }, {}, q)), {})
  end)

  T.it("honors an optional inherit for a language registered for the filetype", function()
    local H = fresh()
    local q = queries({ svelte = { injections = "; inherits: (css)" } })
    T.eq(
      H.inherit_gaps(deps({ svelte = { "svelte" } }, {}, q)),
      { { ft = "svelte", missing = { { lang = "css", via = "svelte" } } } }
    )
  end)

  T.it("ignores an optional inherit reached through another language", function()
    local H = fresh()
    local q = queries({
      svelte = { highlights = "; inherits: html" },
      html   = { injections = "; inherits: (css)" },
    })
    T.eq(
      H.inherit_gaps(deps({ svelte = { "svelte" } }, {}, q)),
      { { ft = "svelte", missing = { { lang = "html", via = "svelte" } } } }
    )
  end)
end)

T.describe("lib.parsers.health report", function()
  T.it("reports ok when every filetype provides the languages its queries inherit", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "; inherits: html" }, html = { highlights = "(x) @y" } })
    local r = H.report(deps({ svelte = { "svelte", "html" } }, {}, q))
    T.eq(#r, 1)
    T.eq(r[1].kind, "ok")
  end)

  T.it("warns naming the filetype, the missing language and what pulled it in", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "; inherits: html" } })
    local r = H.report(deps({ svelte = { "svelte" } }, {}, q))
    T.eq(#r, 1)
    T.eq(r[1].kind, "warn")
    T.truthy(r[1].text:match("svelte"))
    T.truthy(r[1].text:match("html"))
  end)

  T.it("warns once per filetype with a gap", function()
    local H = fresh()
    local q = queries({
      svelte = { highlights = "; inherits: html" },
      vue    = { highlights = "; inherits: html_tags" },
      astro  = { highlights = "(x) @y" },
    })
    local r = H.report(deps({ svelte = { "svelte" }, vue = { "vue" }, astro = { "astro" } }, {}, q))
    T.eq(#r, 2)
    T.eq(r[1].kind, "warn")
    T.eq(r[2].kind, "warn")
  end)
end)

