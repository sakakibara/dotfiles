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

local function deps(registry, requires, query_dir, opts)
  opts = opts or {}
  return {
    fts = function()
      local out = vim.tbl_keys(registry)
      table.sort(out)
      return out
    end,
    parsers_for = function(ft) return registry[ft] or {} end,
    requires = function(lang) return (requires or {})[lang] or {} end,
    query_dir = query_dir,
    buffer_fts = function() return opts.buffer_fts or {} end,
    lang_for_ft = function(ft) return (opts.lang_of or {})[ft] or ft end,
    parser_available = function(lang) return (opts.available or {})[lang] == true end,
  }
end

local function texts(items, kind)
  local out = {}
  for _, item in ipairs(items) do
    if item.kind == kind then out[#out + 1] = item.text end
  end
  return out
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
  T.it("reports no warning when every filetype provides the languages its queries inherit", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "; inherits: html" }, html = { highlights = "(x) @y" } })
    T.eq(texts(H.report(deps({ svelte = { "svelte", "html" } }, {}, q)), "warn"), {})
  end)

  T.it("warns naming the filetype, the missing language and what pulled it in", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "; inherits: html" } })
    local warns = texts(H.report(deps({ svelte = { "svelte" } }, {}, q)), "warn")
    T.eq(#warns, 1)
    T.truthy(warns[1]:match("svelte"))
    T.truthy(warns[1]:match("html"))
  end)

  T.it("warns once per filetype with an inherit gap", function()
    local H = fresh()
    local q = queries({
      svelte = { highlights = "; inherits: html" },
      vue    = { highlights = "; inherits: html_tags" },
      astro  = { highlights = "(x) @y" },
    })
    local r = H.report(deps({ svelte = { "svelte" }, vue = { "vue" }, astro = { "astro" } }, {}, q))
    T.eq(#texts(r, "warn"), 2)
  end)

  T.it("warns naming an open filetype whose parser is available but unregistered", function()
    local H = fresh()
    local q = queries({ svelte = { highlights = "(x) @y" } })
    local warns = texts(H.report(deps({ svelte = { "svelte" } }, {}, q, {
      buffer_fts = { "xml" },
      available = { xml = true },
    })), "warn")
    T.eq(#warns, 1)
    T.truthy(warns[1]:match("xml"))
  end)
end)

local function bdeps(buffer_fts, registry, lang_of, available)
  return {
    buffer_fts = function() return buffer_fts end,
    parsers_for = function(ft) return (registry or {})[ft] or {} end,
    lang_for_ft = function(ft) return (lang_of or {})[ft] or ft end,
    parser_available = function(lang) return (available or {})[lang] == true end,
  }
end

T.describe("lib.parsers.health buffer_gaps", function()
  T.it("reports a filetype that has a parser available but none registered", function()
    local H = fresh()
    T.eq(H.buffer_gaps(bdeps({ "html" }, {}, {}, { html = true })), { { ft = "html", lang = "html" } })
  end)

  T.it("reports nothing when the filetype already has a registered parser", function()
    local H = fresh()
    T.eq(H.buffer_gaps(bdeps({ "html" }, { html = { "html" } }, {}, { html = true })), {})
  end)

  T.it("reports nothing when no parser exists for the filetype", function()
    local H = fresh()
    T.eq(H.buffer_gaps(bdeps({ "log" }, {}, {}, {})), {})
  end)

  T.it("reports a filetype once however many buffers share it", function()
    local H = fresh()
    T.eq(H.buffer_gaps(bdeps({ "html", "html", "html" }, {}, {}, { html = true })), { { ft = "html", lang = "html" } })
  end)

  T.it("names the parser that serves the filetype when they differ", function()
    local H = fresh()
    T.eq(
      H.buffer_gaps(bdeps({ "javascriptreact" }, {}, { javascriptreact = "javascript" }, { javascript = true })),
      { { ft = "javascriptreact", lang = "javascript" } }
    )
  end)

  T.it("sorts by filetype", function()
    local H = fresh()
    T.eq(
      H.buffer_gaps(bdeps({ "xml", "cs" }, {}, { cs = "c_sharp" }, { xml = true, c_sharp = true })),
      { { ft = "cs", lang = "c_sharp" }, { ft = "xml", lang = "xml" } }
    )
  end)
end)

