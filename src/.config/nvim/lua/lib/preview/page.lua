local M = {}

M.VENDOR = {
  katex = { js = "dist/katex.min.js", css = "dist/katex.min.css" },
  mermaid = { js = "dist/mermaid.min.js" },
}

local CSS = [[
:root {
  color-scheme: light;
  --fg: #1f2328; --bg: #ffffff; --muted: #59636e; --border: #d1d9e0; --border-muted: #d1d9e0b3;
  --code-bg: #818b981f; --pre-bg: #f6f8fa; --row: #f6f8fa; --link: #0969da;
  --note: #0969da; --tip: #1a7f37; --important: #8250df; --warning: #9a6700; --caution: #cf222e;
}
:root[data-theme="dark"] {
  color-scheme: dark;
  --fg: #f0f6fc; --bg: #0d1117; --muted: #9198a1; --border: #3d444d; --border-muted: #3d444db3;
  --code-bg: #656c7633; --pre-bg: #151b23; --row: #151b23; --link: #4493f8;
  --note: #4493f8; --tip: #3fb950; --important: #ab7df8; --warning: #d29922; --caution: #f85149;
}
html, body { margin: 0; background: var(--bg); color: var(--fg); }
.markdown-body {
  box-sizing: border-box; max-width: 980px; margin: 0 auto; padding: 32px 45px;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", "Noto Sans", Helvetica, Arial, sans-serif,
    "Apple Color Emoji", "Segoe UI Emoji";
  font-size: 16px; line-height: 1.5; word-wrap: break-word;
}
@media (max-width: 767px) { .markdown-body { padding: 16px; } }
.markdown-body > :first-child { margin-top: 0 !important; }
.markdown-body > :last-child { margin-bottom: 0 !important; }
.markdown-body p, .markdown-body blockquote, .markdown-body ul, .markdown-body ol, .markdown-body dl,
.markdown-body table, .markdown-body pre, .markdown-body details, .markdown-body .markdown-alert,
.markdown-body .math-display { margin-top: 0; margin-bottom: 16px; }
.markdown-body a { color: var(--link); text-decoration: none; }
.markdown-body a:hover { text-decoration: underline; }
.markdown-body h1, .markdown-body h2, .markdown-body h3, .markdown-body h4, .markdown-body h5, .markdown-body h6 {
  position: relative; margin-top: 24px; margin-bottom: 16px; font-weight: 600; line-height: 1.25;
}
.markdown-body h1 { font-size: 2em; padding-bottom: .3em; border-bottom: 1px solid var(--border-muted); }
.markdown-body h2 { font-size: 1.5em; padding-bottom: .3em; border-bottom: 1px solid var(--border-muted); }
.markdown-body h3 { font-size: 1.25em; }
.markdown-body h4 { font-size: 1em; }
.markdown-body h5 { font-size: .875em; }
.markdown-body h6 { font-size: .85em; color: var(--muted); }
.markdown-body .anchor {
  position: absolute; left: -20px; width: 20px; color: var(--muted); opacity: 0; font-weight: 400;
}
.markdown-body h1:hover .anchor, .markdown-body h2:hover .anchor, .markdown-body h3:hover .anchor,
.markdown-body h4:hover .anchor, .markdown-body h5:hover .anchor, .markdown-body h6:hover .anchor {
  opacity: 1; text-decoration: none;
}
.markdown-body hr { height: .25em; padding: 0; margin: 24px 0; background: var(--border); border: 0; }
.markdown-body blockquote { margin-left: 0; margin-right: 0; padding: 0 1em; color: var(--muted); border-left: .25em solid var(--border); }
.markdown-body blockquote > :last-child { margin-bottom: 0; }
.markdown-body ul, .markdown-body ol { padding-left: 2em; }
.markdown-body ul ul, .markdown-body ul ol, .markdown-body ol ol, .markdown-body ol ul { margin-top: 0; margin-bottom: 0; }
.markdown-body li + li { margin-top: .25em; }
.markdown-body li > p { margin-top: 16px; }
.markdown-body .task-list-item { list-style-type: none; }
.markdown-body .task-list-item-checkbox { margin: 0 .2em .25em -1.4em; vertical-align: middle; }
.markdown-body code, .markdown-body pre, .markdown-body kbd {
  font-family: ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace;
}
.markdown-body code {
  padding: .2em .4em; margin: 0; font-size: 85%; white-space: break-spaces; background: var(--code-bg); border-radius: 6px;
}
.markdown-body pre {
  padding: 16px; overflow: auto; font-size: 85%; line-height: 1.45; background: var(--pre-bg); border-radius: 6px;
}
.markdown-body pre code { padding: 0; margin: 0; font-size: 100%; white-space: pre; background: transparent; border: 0; }
.markdown-body pre.mermaid { background: transparent; text-align: center; font-size: 100%; }
.markdown-body pre.mermaid.error { text-align: left; color: var(--caution); }
.markdown-body kbd {
  display: inline-block; padding: 3px 5px; font-size: 11px; line-height: 10px; vertical-align: middle;
  background: var(--pre-bg); border: 1px solid var(--border); border-radius: 6px;
}
.markdown-body table {
  display: block; width: max-content; max-width: 100%; overflow: auto; border-spacing: 0; border-collapse: collapse;
}
.markdown-body th, .markdown-body td { padding: 6px 13px; border: 1px solid var(--border); }
.markdown-body th { font-weight: 600; }
.markdown-body tbody tr:nth-child(2n) { background: var(--row); }
.markdown-body table.front-matter { font-size: 85%; }
.markdown-body img { max-width: 100%; box-sizing: content-box; background: transparent; }
.markdown-body .markdown-alert { padding: .5rem 1rem; color: inherit; border-left: .25em solid var(--border); }
.markdown-body .markdown-alert > :last-child { margin-bottom: 0; }
.markdown-body .markdown-alert-title { margin-bottom: 8px; font-weight: 500; line-height: 1; }
.markdown-body .markdown-alert-note { border-left-color: var(--note); }
.markdown-body .markdown-alert-note .markdown-alert-title { color: var(--note); }
.markdown-body .markdown-alert-tip { border-left-color: var(--tip); }
.markdown-body .markdown-alert-tip .markdown-alert-title { color: var(--tip); }
.markdown-body .markdown-alert-important { border-left-color: var(--important); }
.markdown-body .markdown-alert-important .markdown-alert-title { color: var(--important); }
.markdown-body .markdown-alert-warning { border-left-color: var(--warning); }
.markdown-body .markdown-alert-warning .markdown-alert-title { color: var(--warning); }
.markdown-body .markdown-alert-caution { border-left-color: var(--caution); }
.markdown-body .markdown-alert-caution .markdown-alert-title { color: var(--caution); }
.markdown-body .math-display { display: block; overflow-x: auto; text-align: center; }
.markdown-body .footnotes {
  margin-top: 32px; padding-top: 16px; font-size: 12px; color: var(--muted); border-top: 1px solid var(--border);
}
.markdown-body .footnotes ol { padding-left: 16px; }
.markdown-body sup > a[data-footnote-ref] { font-size: 75%; }
#mdp-badge {
  position: fixed; top: 8px; right: 8px; z-index: 10; padding: 2px 8px; border-radius: 6px;
  font: 12px -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; color: #ffffff; background: var(--caution);
}
#mdp-badge[hidden] { display: none; }
]]

M.JS = [[
(function () {
  "use strict";
  var me = document.currentScript;
  var nonce = me ? me.nonce : "";
  var cfg = JSON.parse(document.getElementById("mdp-cfg").textContent);
  var content = document.getElementById("mdp-content");
  var badge = document.getElementById("mdp-badge");
  var dark = cfg.dark;
  var loads = {};
  var diagramCache = {};
  var diagramTheme = null;
  var seq = 0;

  function load(kind, url) {
    if (!url) return Promise.reject(new Error("not installed"));
    if (loads[url]) return loads[url];
    loads[url] = new Promise(function (resolve, reject) {
      var el = document.createElement(kind);
      if (kind === "script") el.nonce = nonce;
      el.onload = resolve;
      el.onerror = function () { delete loads[url]; reject(new Error("failed to load " + url)); };
      if (kind === "script") { el.src = url; } else { el.rel = "stylesheet"; el.href = url; }
      document.head.appendChild(el);
    });
    return loads[url];
  }

  function math() {
    var nodes = content.querySelectorAll(".math-inline, .math-display");
    if (!nodes.length) return;
    if (!cfg.katex) return;
    load("link", cfg.katex.css).catch(function () {});
    load("script", cfg.katex.js).then(function () {
      nodes.forEach(function (el) {
        if (!el.isConnected || el.getAttribute("data-tex") !== null) return;
        var tex = el.textContent;
        el.setAttribute("data-tex", tex);
        try {
          window.katex.render(tex, el, { displayMode: el.classList.contains("math-display"), throwOnError: false });
        } catch (e) {
          el.textContent = tex;
        }
      });
    }).catch(function () {});
  }

  function diagrams() {
    var nodes = content.querySelectorAll("pre.mermaid");
    if (!nodes.length) return;
    if (!cfg.mermaid) return;
    load("script", cfg.mermaid.js).then(function () {
      var m = window.mermaid;
      if (diagramTheme !== dark) {
        m.initialize({ startOnLoad: false, securityLevel: "strict", theme: dark ? "dark" : "default" });
        diagramTheme = dark;
        diagramCache = {};
      }
      nodes.forEach(function (el) {
        if (!el.isConnected) return;
        var src = el.getAttribute("data-src");
        if (src === null) { src = el.textContent; el.setAttribute("data-src", src); }
        if (diagramCache[src]) { el.innerHTML = diagramCache[src]; return; }
        seq += 1;
        var id = "mdp-mermaid-" + seq;
        m.render(id, src).then(function (r) {
          diagramCache[src] = r.svg;
          if (el.isConnected) { el.classList.remove("error"); el.innerHTML = r.svg; }
        }).catch(function (e) {
          var stray = document.getElementById("d" + id);
          if (stray) stray.remove();
          el.classList.add("error");
          el.textContent = src + "\n" + String((e && e.message) || e);
        });
      });
    }).catch(function () {});
  }

  function post() { math(); diagrams(); }

  function cursor(line) {
    var nodes = content.querySelectorAll("[data-line]");
    var best = null, bestLine = -1;
    for (var i = 0; i < nodes.length; i++) {
      var l = Number(nodes[i].getAttribute("data-line"));
      if (l <= line && l >= bestLine) { best = nodes[i]; bestLine = l; }
    }
    if (!best) { window.scrollTo({ top: 0, behavior: "smooth" }); return; }
    var r = best.getBoundingClientRect();
    var h = Math.min(r.height, window.innerHeight);
    window.scrollTo({ top: window.scrollY + r.top - (window.innerHeight - h) / 2, behavior: "smooth" });
  }

  document.addEventListener("click", function (ev) {
    var a = ev.target && ev.target.closest ? ev.target.closest("a[href^='#']") : null;
    if (!a) return;
    ev.preventDefault();
    var id = a.getAttribute("href").slice(1);
    try { id = decodeURIComponent(id); } catch (e) {}
    var el = document.getElementById(id);
    if (el) el.scrollIntoView({ behavior: "smooth", block: "start" });
  });

  var es = new EventSource(cfg.events);
  es.addEventListener("open", function () { badge.hidden = true; });
  es.addEventListener("error", function () { badge.hidden = false; });
  es.addEventListener("render", function (ev) {
    var d = JSON.parse(ev.data);
    var y = window.scrollY;
    content.innerHTML = d.html;
    window.scrollTo(0, y);
    post();
  });
  es.addEventListener("cursor", function (ev) { cursor(JSON.parse(ev.data).line); });
  es.addEventListener("theme", function (ev) {
    var d = JSON.parse(ev.data);
    dark = d.dark;
    document.documentElement.setAttribute("data-theme", dark ? "dark" : "light");
    document.getElementById("mdp-hl").textContent = d.css;
    content.querySelectorAll("pre.mermaid[data-src]").forEach(function (el) { el.textContent = el.getAttribute("data-src"); });
    diagrams();
  });
  post();
})();
]]

local function hex(n)
  return n and string.format("#%06x", n) or nil
end

local function resolve(groups, name)
  local g = groups[name]
  for _ = 1, 20 do
    if not (g and g.link) then break end
    g = groups[g.link]
  end
  return g
end

local function decls(g)
  local d = {}
  if g.fg then d[#d + 1] = "color: " .. hex(g.fg) end
  if g.bg then d[#d + 1] = "background-color: " .. hex(g.bg) end
  if g.bold then d[#d + 1] = "font-weight: bold" end
  if g.italic then d[#d + 1] = "font-style: italic" end
  local lines = {}
  if g.underline or g.undercurl or g.underdouble or g.underdotted or g.underdashed then lines[#lines + 1] = "underline" end
  if g.strikethrough then lines[#lines + 1] = "line-through" end
  if #lines > 0 then d[#d + 1] = "text-decoration: " .. table.concat(lines, " ") end
  return d
end

function M.theme()
  local groups = vim.api.nvim_get_hl(0, {})
  local names = {}
  for name in pairs(groups) do
    if name:sub(1, 1) == "@" then names[#names + 1] = name end
  end
  local function depth(n)
    return select(2, n:gsub("%.", ""))
  end
  table.sort(names, function(a, b)
    local da, db = depth(a), depth(b)
    if da ~= db then return da < db end
    return a < b
  end)
  local out = {}
  local normal = resolve(groups, "Normal")
  if normal and (normal.fg or normal.bg) then
    local d = {}
    if normal.bg then d[#d + 1] = "background-color: " .. hex(normal.bg) end
    if normal.fg then d[#d + 1] = "color: " .. hex(normal.fg) end
    out[#out + 1] = ".markdown-body pre:not(.mermaid) { " .. table.concat(d, "; ") .. " }"
  end
  for _, name in ipairs(names) do
    local g = resolve(groups, name)
    local d = g and decls(g) or {}
    if #d > 0 then
      local cls = "hl-" .. name:sub(2):gsub("%.", "-"):gsub("[^%w_%-]", "_")
      out[#out + 1] = ".markdown-body pre ." .. cls .. " { " .. table.concat(d, "; ") .. " }"
    end
  end
  return { dark = vim.o.background == "dark", css = table.concat(out, "\n") }
end

local function json_script(v)
  return (vim.json.encode(v):gsub("<", "\\u003c"):gsub(">", "\\u003e"):gsub("&", "\\u0026"))
end

function M.html(o)
  local vendor = o.vendor or {}
  local cfg = json_script({
    events = o.events, dark = o.theme.dark, katex = vendor.katex or vim.NIL, mermaid = vendor.mermaid or vim.NIL,
  })
  local css = o.theme.css:gsub("</", "<\\/")
  return table.concat({
    "<!doctype html>",
    '<html lang="en" data-theme="' .. (o.theme.dark and "dark" or "light") .. '">',
    "<head>",
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    "<title>" .. o.escape(o.title) .. "</title>",
    '<base href="' .. o.escape(o.base) .. '">',
    '<link rel="icon" href="data:,">',
    "<style>" .. CSS .. "</style>",
    '<style id="mdp-hl">' .. css .. "</style>",
    '<script type="application/json" id="mdp-cfg">' .. cfg .. "</script>",
    '<script defer nonce="' .. o.nonce .. '" src="' .. o.escape(o.script) .. '"></script>',
    "</head>",
    "<body>",
    '<div id="mdp-badge" hidden>disconnected</div>',
    '<article id="mdp-content" class="markdown-body">',
    o.content,
    "</article>",
    "</body>",
    "</html>",
    "",
  }, "\n")
end

function M.csp(nonce)
  return "default-src 'none'; script-src 'nonce-" .. nonce .. "' 'strict-dynamic'; style-src 'self' 'unsafe-inline'; "
    .. "font-src 'self'; img-src 'self' data: https:; connect-src 'self'"
end

return M
