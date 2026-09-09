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

local function deps(opts)
  local registry = opts.registry or {}
  return {
    fts = function()
      local fts = vim.tbl_keys(registry)
      table.sort(fts)
      return fts
    end,
    parsers_for = function(ft) return registry[ft] or {} end,
    requires = function(lang) return (opts.requires or {})[lang] or {} end,
    query_dir = opts.query_dir,
    buffer_fts = function() return opts.buffer_fts or {} end,
    lang_for_ft = function(ft) return (opts.langs or {})[ft] or ft end,
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
    local gaps = H.inherit_gaps(deps({
      registry = { svelte = { "svelte" } },
      query_dir = queries({ svelte = { highlights = "; inherits: html" } }),
    }))
    T.eq(gaps, { { ft = "svelte", missing = { { lang = "html", via = "svelte" } } } })
  end)

  T.it("reports nothing when the inherited language is registered for the filetype", function()
    local H = fresh()
    T.eq(H.inherit_gaps(deps({
      registry = { svelte = { "svelte", "html" } },
      query_dir = queries({ svelte = { highlights = "; inherits: html" }, html = { highlights = "(x) @y" } }),
    })), {})
  end)

  T.it("treats a parser pulled in by requires as providing the inherited language", function()
    local H = fresh()
    T.eq(H.inherit_gaps(deps({
      registry = { svelte = { "svelte" } },
      requires = { svelte = { "html_tags" } },
      query_dir = queries({ svelte = { injections = "; inherits: html_tags" }, html_tags = { highlights = "(x) @y" } }),
    })), {})
  end)

  T.it("follows inheritance transitively through languages that are themselves missing", function()
    local H = fresh()
    local gaps = H.inherit_gaps(deps({
      registry = { svelte = { "svelte" } },
      query_dir = queries({
        svelte    = { highlights = "; inherits: html" },
        html      = { highlights = "; inherits: html_tags" },
        html_tags = { highlights = "(x) @y" },
      }),
    }))
    T.eq(gaps, {
      { ft = "svelte", missing = { { lang = "html", via = "svelte" }, { lang = "html_tags", via = "html" } } },
    })
  end)

  T.it("ignores a query that inherits its own language", function()
    local H = fresh()
    T.eq(H.inherit_gaps(deps({
      registry = { lua = { "lua" } },
      query_dir = queries({ lua = { highlights = "; inherits: lua" } }),
    })), {})
  end)

  T.it("does not report a language the missing language's own requires would provide", function()
    local H = fresh()
    local gaps = H.inherit_gaps(deps({
      registry = { svelte = { "svelte" } },
      requires = { html = { "html_tags" } },
      query_dir = queries({
        svelte    = { highlights = "; inherits: html" },
        html      = { highlights = "; inherits: html_tags" },
        html_tags = { highlights = "(x) @y" },
      }),
    }))
    T.eq(gaps, { { ft = "svelte", missing = { { lang = "html", via = "svelte" } } } })
  end)

  T.it("reads an inherits modeline below other leading comment lines", function()
    local H = fresh()
    local gaps = H.inherit_gaps(deps({
      registry = { svelte = { "svelte" } },
      query_dir = queries({ svelte = { highlights = "; a note\n; inherits: html" } }),
    }))
    T.eq(gaps, { { ft = "svelte", missing = { { lang = "html", via = "svelte" } } } })
  end)

  T.it("stops scanning at the first line that is not a comment", function()
    local H = fresh()
    T.eq(H.inherit_gaps(deps({
      registry = { svelte = { "svelte" } },
      query_dir = queries({ svelte = { highlights = "(x) @y\n; inherits: html" } }),
    })), {})
  end)

  T.it("honors an optional inherit for a language registered for the filetype", function()
    local H = fresh()
    local gaps = H.inherit_gaps(deps({
      registry = { svelte = { "svelte" } },
      query_dir = queries({ svelte = { injections = "; inherits: (css)" } }),
    }))
    T.eq(gaps, { { ft = "svelte", missing = { { lang = "css", via = "svelte" } } } })
  end)

  T.it("ignores an optional inherit reached through another language", function()
    local H = fresh()
    local gaps = H.inherit_gaps(deps({
      registry = { svelte = { "svelte" } },
      query_dir = queries({
        svelte = { highlights = "; inherits: html" },
        html   = { injections = "; inherits: (css)" },
      }),
    }))
    T.eq(gaps, { { ft = "svelte", missing = { { lang = "html", via = "svelte" } } } })
  end)
end)

T.describe("lib.parsers.health report", function()
  T.it("reports no warning when every filetype provides the languages its queries inherit", function()
    local H = fresh()
    local report = H.report(deps({
      registry = { svelte = { "svelte", "html" } },
      query_dir = queries({ svelte = { highlights = "; inherits: html" }, html = { highlights = "(x) @y" } }),
    }))
    T.eq(texts(report, "warn"), {})
  end)

  T.it("warns naming the filetype, the missing language and what pulled it in", function()
    local H = fresh()
    local warns = texts(H.report(deps({
      registry = { svelte = { "svelte" } },
      query_dir = queries({ svelte = { highlights = "; inherits: html" } }),
    })), "warn")
    T.eq(#warns, 1)
    T.truthy(warns[1]:match("svelte"))
    T.truthy(warns[1]:match("html"))
  end)

  T.it("warns once per filetype with an inherit gap", function()
    local H = fresh()
    local report = H.report(deps({
      registry = { svelte = { "svelte" }, vue = { "vue" }, astro = { "astro" } },
      query_dir = queries({
        svelte = { highlights = "; inherits: html" },
        vue    = { highlights = "; inherits: html_tags" },
        astro  = { highlights = "(x) @y" },
      }),
    }))
    T.eq(#texts(report, "warn"), 2)
  end)

  T.it("warns naming an open filetype whose parser is available but unregistered", function()
    local H = fresh()
    local warns = texts(H.report(deps({
      registry = { svelte = { "svelte" } },
      query_dir = queries({ svelte = { highlights = "(x) @y" } }),
      buffer_fts = { "xml" },
      available = { xml = true },
    })), "warn")
    T.eq(#warns, 1)
    T.truthy(warns[1]:match("xml"))
  end)
end)

T.describe("lib.parsers.health orphan_gaps", function()
  T.it("reports a filetype registered with sibling parsers only, installed or not", function()
    local H = fresh()
    T.eq(
      H.orphan_gaps(deps({ registry = { cs = { "c_sharp", "razor" } } })),
      { { ft = "cs", lang = "cs" } }
    )
    T.eq(
      H.orphan_gaps(deps({ registry = { cs = { "c_sharp", "razor" } }, available = { c_sharp = true } })),
      { { ft = "cs", lang = "cs" } }
    )
  end)

  T.it("leaves a filetype alone when no parser is registered for it", function()
    local H = fresh()
    T.eq(H.orphan_gaps(deps({ registry = { vb = {}, ocaml = { "ocaml" } } })), {})
  end)

  T.it("reports nothing when the filetype's own language is registered", function()
    local H = fresh()
    T.eq(H.orphan_gaps(deps({ registry = { cs = { "c_sharp", "razor" } }, langs = { cs = "c_sharp" } })), {})
  end)

  T.it("accepts a filetype served through a registered alias", function()
    local H = fresh()
    T.eq(H.orphan_gaps(deps({ registry = { mysql = { "sql" } }, langs = { mysql = "sql" } })), {})
  end)

  T.it("is reported by the health check", function()
    local H = fresh()
    local warns = texts(H.report(deps({
      registry = { vb = { "c_sharp" } },
      available = { vb = true },
      query_dir = queries({ c_sharp = { highlights = "(x) @y" } }),
    })), "warn")
    T.eq(#warns, 1)
    T.truthy(warns[1]:match("vb"))
  end)
end)

T.describe("lib.parsers.health buffer_gaps", function()
  T.it("reports a filetype that has a parser available but none registered", function()
    local H = fresh()
    T.eq(
      H.buffer_gaps(deps({ buffer_fts = { "html" }, available = { html = true } })),
      { { ft = "html", lang = "html" } }
    )
  end)

  T.it("reports nothing when the filetype already has a registered parser", function()
    local H = fresh()
    T.eq(H.buffer_gaps(deps({
      registry = { html = { "html" } },
      buffer_fts = { "html" },
      available = { html = true },
    })), {})
  end)

  T.it("reports a filetype whose registered parsers exclude the one serving it", function()
    local H = fresh()
    T.eq(
      H.buffer_gaps(deps({
        registry = { eruby = { "ruby" } },
        buffer_fts = { "eruby" },
        langs = { eruby = "embedded_template" },
        available = { embedded_template = true },
      })),
      { { ft = "eruby", lang = "embedded_template" } }
    )
  end)

  T.it("reports nothing when no parser exists for the filetype", function()
    local H = fresh()
    T.eq(H.buffer_gaps(deps({ buffer_fts = { "log" } })), {})
  end)

  T.it("reports a filetype once however many buffers share it", function()
    local H = fresh()
    T.eq(
      H.buffer_gaps(deps({ buffer_fts = { "html", "html", "html" }, available = { html = true } })),
      { { ft = "html", lang = "html" } }
    )
  end)

  T.it("names the parser that serves the filetype when they differ", function()
    local H = fresh()
    T.eq(
      H.buffer_gaps(deps({
        buffer_fts = { "javascriptreact" },
        langs = { javascriptreact = "javascript" },
        available = { javascript = true },
      })),
      { { ft = "javascriptreact", lang = "javascript" } }
    )
  end)

  T.it("sorts by filetype", function()
    local H = fresh()
    T.eq(
      H.buffer_gaps(deps({
        buffer_fts = { "xml", "cs" },
        langs = { cs = "c_sharp" },
        available = { xml = true, c_sharp = true },
      })),
      { { ft = "cs", lang = "c_sharp" }, { ft = "xml", lang = "xml" } }
    )
  end)
end)
