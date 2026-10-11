local T = require("tests.helpers")

local R = require("lib.preview.render")

local function render(lines)
  return R.render_lines(lines)
end

local function has(html, needle)
  T.truthy(html:find(needle, 1, true), "missing " .. vim.inspect(needle) .. " in\n" .. html)
end

local function lacks(html, needle)
  T.truthy(not html:find(needle, 1, true), "unexpected " .. vim.inspect(needle) .. " in\n" .. html)
end

T.describe("lib.preview.render headings", function()
  T.it("renders atx and setext headings with GitHub ids, deduped, and an anchor", function()
    T.eq(render({ "# Intro", "## Intro", "Intro", "-----" }), table.concat({
      '<h1 id="intro" data-line="0"><a class="anchor" href="#intro" aria-hidden="true">#</a>Intro</h1>',
      '<h2 id="intro-1" data-line="1"><a class="anchor" href="#intro-1" aria-hidden="true">#</a>Intro</h2>',
      '<h2 id="intro-2" data-line="2"><a class="anchor" href="#intro-2" aria-hidden="true">#</a>Intro</h2>',
      "",
    }, "\n"))
  end)

  T.it("slugs the heading's text, not its markup", function()
    has(render({ "## The `run` *command*" }), 'id="the-run-command"')
  end)
end)

T.describe("lib.preview.render blocks", function()
  T.it("renders a GitHub alert from a block quote whose first line is the marker", function()
    T.eq(render({ "> [!WARNING]", "> Be careful" }), table.concat({
      '<div class="markdown-alert markdown-alert-warning" data-line="0"><p class="markdown-alert-title">Warning</p>',
      '<p data-line="1">Be careful</p>',
      "</div>",
      "",
    }, "\n"))
    has(render({ "> [!tip]", "> lower" }), 'class="markdown-alert markdown-alert-tip"')
  end)

  T.it("keeps a plain block quote when the marker is not alone on its line", function()
    local html = render({ "> [!NOTE] not alone" })
    has(html, "<blockquote")
    lacks(html, "markdown-alert")
  end)

  T.it("renders task lists with disabled checkboxes", function()
    T.eq(render({ "- [ ] a", "- [x] b" }), table.concat({
      '<ul class="contains-task-list" data-line="0">',
      '<li class="task-list-item" data-line="0"><input type="checkbox" class="task-list-item-checkbox" disabled> a</li>',
      '<li class="task-list-item" data-line="1"><input type="checkbox" class="task-list-item-checkbox" disabled checked> b</li>',
      "</ul>",
      "",
    }, "\n"))
  end)

  T.it("renders tight lists without paragraphs and loose ones with them", function()
    T.eq(render({ "1. one", "2. two" }), '<ol data-line="0">\n<li data-line="0">one</li>\n<li data-line="1">two</li>\n</ol>\n')
    has(render({ "- loose", "", "- list" }), '<li data-line="2"><p data-line="2">list</p></li>')
  end)

  T.it("carries an ordered list's start number", function()
    has(render({ "3. three", "4. four" }), '<ol start="3" data-line="0">')
  end)

  T.it("renders pipe tables with alignment, escaped pipes and missing cells", function()
    local html = render({
      "| Left | Right | Center | None |",
      "|:-----|------:|:------:|------|",
      "| `a \\| b` | x \\| y | | z |",
      "| only |",
    })
    has(html, '<th align="left">Left</th>\n<th align="right">Right</th>\n<th align="center">Center</th>\n<th>None</th>')
    has(html, '<td align="left"><code>a | b</code></td>\n<td align="right">x | y</td>\n<td align="center"></td>\n<td>z</td>')
    has(html, '<tr data-line="3">\n<td align="left">only</td>\n<td align="right"></td>\n<td align="center"></td>\n<td></td>')
  end)

  T.it("renders thematic breaks and indented code", function()
    T.eq(render({ "Para", "***", "    indented", "    code" }), table.concat({
      '<p data-line="0">Para</p>',
      '<hr data-line="1">',
      '<pre data-line="2"><code>indented',
      "code",
      "</code></pre>",
      "",
    }, "\n"))
  end)

  T.it("keeps block continuation markers out of quoted and listed text", function()
    local html = render({ "> quote *across", "> lines* end", "> - item", ">   continued" })
    has(html, '<p data-line="0">quote <em>across\nlines</em> end</p>')
    has(html, '<li data-line="2">item\ncontinued</li>')
    html = render({ "- item with **bold", "  across** lines", "  - nested *em", "    spans*" })
    has(html, "<strong>bold\nacross</strong> lines")
    has(html, "nested <em>em\nspans</em>")
  end)

  T.it("passes html blocks and inline tags through", function()
    local html = render({ "<details>", "<summary>More</summary>", "", "Hidden *text*", "", "</details>" })
    has(html, "<details>\n<summary>More</summary>")
    has(html, '<p data-line="3">Hidden <em>text</em></p>')
    has(render({ 'a <span class="x">raw</span>' }), '<span class="x">raw</span>')
  end)

  T.it("renders yaml and toml front matter as a table of top-level keys", function()
    local html = render({ "---", "title: Doc", "tags: [a, b]", "---", "# Body" })
    has(html, '<table class="front-matter" data-line="0">')
    has(html, "<th>title</th>\n<th>tags</th>")
    has(html, "<td>Doc</td>\n<td>[a, b]</td>")
    has(html, '<h1 id="body" data-line="4">')
    has(render({ "+++", 'title = "T"', "+++" }), "<th>title</th>")
  end)

  T.it("omits front matter that is not a simple mapping", function()
    T.eq(render({ "---", "- not", "- a mapping", "---", "text" }), '<p data-line="4">text</p>\n')
  end)

  T.it("marks every block with its 0-based start line", function()
    local html = render({ "# A", "", "para", "", "- x", "", "```", "c", "```", "", "> q" })
    has(html, '<h1 id="a" data-line="0">')
    has(html, '<p data-line="2">para</p>')
    has(html, '<ul data-line="4">')
    has(html, '<pre data-line="6">')
    has(html, '<blockquote data-line="10">')
  end)
end)

T.describe("lib.preview.render inline", function()
  T.it("renders emphasis, strong, strikethrough, code spans and hard breaks", function()
    T.eq(render({ "***bold italic*** ~~gone~~ ~single~ `` code ` tick `` a\\", "b" }),
      '<p data-line="0"><em><strong>bold italic</strong></em> <del>gone</del> <del>single</del> '
        .. "<code>code ` tick</code> a<br>\nb</p>\n")
  end)

  T.it("renders uri, email and bare http(s) autolinks, trimming trailing punctuation", function()
    T.eq(render({ "Visit https://example.com/path). or <https://a.example> or <me@example.com>" }),
      '<p data-line="0">Visit <a href="https://example.com/path">https://example.com/path</a>). or '
        .. '<a href="https://a.example">https://a.example</a> or <a href="mailto:me@example.com">me@example.com</a></p>\n')
  end)

  T.it("resolves full, collapsed and shortcut reference links and leaves unknown ones literal", function()
    T.eq(render({ "[ref] and [text][ref] and [ref][] and [nope]", "", '[ref]: https://ref.example/x "Title"' }),
      '<p data-line="0"><a href="https://ref.example/x" title="Title">ref</a> and '
        .. '<a href="https://ref.example/x" title="Title">text</a> and '
        .. '<a href="https://ref.example/x" title="Title">ref</a> and [nope]</p>\n')
  end)

  T.it("renders inline links with titles and images, relative or remote or by reference", function()
    has(render({ '[x](https://a.example "T")' }), '<a href="https://a.example" title="T">x</a>')
    T.eq(render({ '![logo](img/logo.png "L") ![remote](https://x.example/a.png) ![ref img][r]', "", "[r]: pic.svg" }),
      '<p data-line="0"><img src="img/logo.png" alt="logo" title="L"> <img src="https://x.example/a.png" alt="remote"> '
        .. '<img src="pic.svg" alt="ref img"></p>\n')
    has(render({ "![sp](<my pic.png>)" }), 'src="my%20pic.png"')
  end)

  T.it("numbers footnotes by first use, links back to every reference, and leaves unknown ids literal", function()
    T.eq(render({ "See[^1] and[^1] and [^missing].", "", "[^1]: Note *one*." }), table.concat({
      '<p data-line="0">See<sup><a href="#fn-1" id="fnref-1" data-footnote-ref>1</a></sup> and'
        .. '<sup><a href="#fn-1" id="fnref-1-2" data-footnote-ref>1</a></sup> and [^missing].</p>',
      '<section class="footnotes" data-footnotes>',
      "<ol>",
      '<li id="fn-1" data-line="2"><p>Note <em>one</em>. '
        .. '<a href="#fnref-1" class="data-footnote-backref" aria-label="Back to reference 1">&#8617;</a> '
        .. '<a href="#fnref-1-2" class="data-footnote-backref" aria-label="Back to reference 1">&#8617;</a></p></li>',
      "</ol>",
      "</section>",
      "",
    }, "\n"))
  end)

  T.it("numbers footnotes in order of first reference, not definition", function()
    local html = render({ "b[^b] a[^a]", "", "[^a]: A", "[^b]: B" })
    has(html, '<li id="fn-1" data-line="3"><p>B ')
    has(html, '<li id="fn-2" data-line="2"><p>A ')
  end)

  T.it("escapes text and attribute values", function()
    T.eq(render({ 'a < b & "c" \'d\'' }), '<p data-line="0">a &lt; b &amp; &quot;c&quot; &#39;d&#39;</p>\n')
    has(render({ "[x](https://a.example/?q=\"'&)" }), 'href="https://a.example/?q=%22&#39;&amp;"')
  end)

  T.it("neutralizes javascript:, vbscript: and non-image data: urls", function()
    T.eq(render({ "[a](javascript:alert(1)) [b](JavaScript:alert(1)) [c](data:text/html,x) ![d](data:image/png;base64,AA) [e](vbscript:x)" }),
      '<p data-line="0"><a href="#">a</a> <a href="#">b</a> <a href="#">c</a> '
        .. '<img src="data:image/png;base64,AA" alt="d"> <a href="#">e</a></p>\n')
    has(render({ "[x](java&#9;script:alert(1))" }), '<a href="#">x</a>')
  end)
end)

T.describe("lib.preview.render code, diagrams and math", function()
  T.it("highlights a fenced block whose language has a parser with capture classes", function()
    local html = render({ "```lua", "local x = 1", "```" })
    has(html, '<pre data-line="0"><code class="language-lua">')
    has(html, '<span class="hl-keyword">local</span>')
    has(html, '<span class="hl-number">1</span>')
  end)

  T.it("emits nested capture names as one class per level", function()
    has(render({ "```lua", "local function f() end", "```" }), 'class="hl-keyword hl-keyword-function"')
  end)

  T.it("escapes a fenced block in an unknown language", function()
    T.eq(render({ "```nosuchlang", '<tag> & "q"', "```" }),
      '<pre data-line="0"><code class="language-nosuchlang">&lt;tag&gt; &amp; &quot;q&quot;\n</code></pre>\n')
  end)

  T.it("wraps mermaid fences for the client", function()
    T.eq(render({ "```mermaid", "graph TD; A-->B", "```" }), '<pre class="mermaid" data-line="0">graph TD; A--&gt;B\n</pre>\n')
  end)

  T.it("wraps inline and display math in either delimiter style", function()
    T.eq(render({ "Inline $a+b$ and \\(c<d\\) and display:", "", "$$", "x^2", "$$", "", "\\[", "y", "\\]" }), table.concat({
      '<p data-line="0">Inline <span class="math-inline">a+b</span> and <span class="math-inline">c&lt;d</span> and display:</p>',
      '<div class="math-display" data-line="2">',
      "x^2",
      "</div>",
      '<div class="math-display" data-line="6">',
      "y",
      "</div>",
      "",
    }, "\n"))
  end)
end)

T.describe("lib.preview.render robustness", function()
  T.it("falls back to the escaped source of a block whose rendering fails and continues", function()
    local orig = vim.treesitter.get_string_parser
    vim.treesitter.get_string_parser = function(s, lang, opts)
      if lang == "markdown_inline" and s:find("boom", 1, true) then error("injected failure") end
      return orig(s, lang, opts)
    end
    local ok, html = pcall(render, { "fine *one*", "", "boom <b>", "", "fine *two*" })
    vim.treesitter.get_string_parser = orig
    T.truthy(ok, html)
    has(html, '<p data-line="0">fine <em>one</em></p>')
    has(html, '<p data-line="2">boom &lt;b&gt;\n</p>')
    has(html, '<p data-line="4">fine <em>two</em></p>')
  end)

  T.it("does not throw on odd input", function()
    local odd = {
      { "\0\1\2", "\255\254" },
      { "> > > > > > deep", ">>>>>> [!NOTE]" },
      { "| a |", "|---|", "| `unclosed |" },
      { "[^", "[^]: x", "[]: y", "![", "<", "&#xFFFFFFFF;", "\\" },
      { "```", "no close" },
      { "- [ ]", "  - [x]", "    1)", "      * " },
    }
    for _, lines in ipairs(odd) do
      local ok, html = pcall(render, lines)
      T.truthy(ok, html)
      T.eq(type(html), "string")
    end
  end)

  T.it("renders a 2000-line README within the time bound", function()
    local block = {
      "## Section heading", "",
      "Some text with **bold**, `code`, a [link](https://example.com) and https://bare.example/path.",
      "A second line with *emphasis* and ~~strike~~ and a footnote[^n].", "",
      "- item one", "- item two with `code`", "  - nested item", "",
      "| a | b |", "|---|--:|", "| 1 | **2** |", "| 3 | 4 |", "",
      "```lua", "local x = { 1, 2, 3 }", "print(x)", "```", "",
      "> [!NOTE]", "> A note with *style*.", "",
    }
    local lines = {}
    while #lines < 2000 do vim.list_extend(lines, block) end
    lines[#lines + 1] = "[^n]: The note."
    render(lines)
    local t0 = vim.uv.hrtime()
    local html = render(lines)
    local ms = (vim.uv.hrtime() - t0) / 1e6
    print(string.format("      (rendered %d lines in %.1f ms)", #lines, ms))
    T.truthy(#html > 0)
    T.truthy(ms < 200, string.format("took %.1f ms", ms))
  end)
end)
