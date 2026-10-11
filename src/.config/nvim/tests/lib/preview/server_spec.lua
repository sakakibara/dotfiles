local T = require("tests.helpers")

local P = require("lib.preview")

local uv = vim.uv
local LIMIT = 3000

local function wait(cond, ms)
  return vim.wait(ms or LIMIT, cond, 5)
end

local function connect(port, raw)
  local c = { data = "", eof = false, tcp = uv.new_tcp() }
  c.tcp:connect("127.0.0.1", port, function(err)
    if err then
      c.err = err
      c.eof = true
      return
    end
    c.connected = true
    if raw then c.tcp:write(raw) end
    c.tcp:read_start(function(e, chunk)
      if e or not chunk then
        c.eof = true
      else
        c.data = c.data .. chunk
      end
    end)
  end)
  return c
end

local function shut(c)
  if not c.tcp:is_closing() then c.tcp:close() end
end

local function parse(data)
  local headend = data:find("\r\n\r\n", 1, true)
  if not headend then return nil end
  local head = data:sub(1, headend - 1)
  local status = tonumber(head:match("^HTTP/1%.1 (%d+)"))
  local headers = {}
  for name, value in head:gmatch("\r\n([%w%-]+): ([^\r\n]*)") do headers[name:lower()] = value end
  return { status = status, headers = headers, body = data:sub(headend + 4) }
end

local function get(port, path, extra)
  local c = connect(port, "GET " .. path .. " HTTP/1.1\r\nHost: 127.0.0.1:" .. port .. "\r\n" .. (extra or "") .. "\r\n")
  T.truthy(wait(function() return c.eof end), "response to " .. path)
  shut(c)
  return parse(c.data) or { status = nil, raw = c.data }
end

local function events_of(data)
  local list = {}
  for name, payload in data:gmatch("event: (%w+)\ndata: ([^\n]*)\n\n") do
    list[#list + 1] = { name = name, data = vim.json.decode(payload) }
  end
  return list
end

local function count(list, name)
  local n = 0
  for _, e in ipairs(list) do
    if e.name == name then n = n + 1 end
  end
  return n
end

local function fixture()
  local root = vim.fn.tempname()
  local dir = root .. "/doc"
  vim.fn.mkdir(dir .. "/img", "p")
  vim.fn.writefile({ "# Title", "", "para", "", "![p](img/p.png)" }, dir .. "/README.md")
  vim.fn.writefile({ "PNGDATA" }, dir .. "/img/p.png", "b")
  vim.fn.writefile({ "not an image" }, dir .. "/notes.txt")
  vim.fn.writefile({ "SECRET" }, root .. "/secret.png", "b")
  uv.fs_symlink(root .. "/secret.png", dir .. "/link.png")
  vim.cmd("edit " .. vim.fn.fnameescape(dir .. "/README.md"))
  return vim.api.nvim_get_current_buf(), root
end

local function open(buf)
  local opened
  local orig = P.open_url
  P.open_url = function(url) opened = url end
  local url = P.open(buf)
  P.open_url = orig
  T.eq(opened, url)
  local port, token = url:match("^http://127%.0%.0%.1:(%d+)/(%x+)/b/%d+$")
  T.truthy(port, url)
  return tonumber(port), token, url
end

local function cleanup(buf)
  P.stop()
  if vim.api.nvim_buf_is_valid(buf) then vim.api.nvim_buf_delete(buf, { force = true }) end
end

local function sse(port, token, buf)
  local c = connect(port, ("GET /%s/b/%d/events HTTP/1.1\r\nHost: localhost:%d\r\nAccept: text/event-stream\r\n\r\n"):format(token, buf, port))
  T.truthy(wait(function() return count(events_of(c.data), "render") >= 1 end), "initial render event")
  return c
end

T.describe("lib.preview server", function()
  T.it("binds 127.0.0.1 on an OS-assigned port with a 128-bit token", function()
    local buf = fixture()
    local port, token = open(buf)
    T.truthy(port > 0)
    T.eq(#token, 32)
    T.eq(P._state().server.tcp:getsockname().ip, "127.0.0.1")
    cleanup(buf)
  end)

  T.it("answers 404 for a wrong token, an unknown buffer or route, without echoing the token", function()
    local buf = fixture()
    local port, token = open(buf)
    local wrong = get(port, ("/%s/b/%d"):format(string.rep("0", 32), buf))
    T.eq(wrong.status, 404)
    T.truthy(not wrong.body:find(token, 1, true))
    T.eq(get(port, ("/%s/b/%d"):format(token, buf + 1000)).status, 404)
    T.eq(get(port, ("/%s/b/%d/nope"):format(token, buf)).status, 404)
    T.eq(get(port, "/").status, 404)
    T.eq(get(port, "/favicon.ico").status, 404)
    cleanup(buf)
  end)

  T.it("serves the page with a per-response nonce in the CSP and the script tag", function()
    local buf = fixture()
    local port, token = open(buf)
    local path = ("/%s/b/%d"):format(token, buf)
    local a, b = get(port, path), get(port, path)
    T.eq(a.status, 200)
    T.eq(a.headers["content-type"], "text/html; charset=utf-8")
    T.eq(a.headers["x-content-type-options"], "nosniff")
    T.eq(a.headers["cache-control"], "no-store")
    local csp = a.headers["content-security-policy"]
    local nonce = csp:match("script%-src 'nonce%-(%x+)'")
    T.truthy(nonce, csp)
    T.truthy(csp:find("default-src 'none'", 1, true), csp)
    T.truthy(csp:find("connect-src 'self'", 1, true), csp)
    local tag = ('<script defer nonce="%s" src="/%s/page.js"></script>'):format(nonce, token)
    local at = a.body:find(tag, 1, true)
    T.truthy(at, "deferred page script")
    T.truthy(at < a.body:find("</head>", 1, true), "page script sits in head")
    T.truthy(a.body:find('<script type="application/json" id="mdp-cfg">', 1, true) < at, "config precedes the script")
    T.truthy(a.body:find("<title>README.md</title>", 1, true))
    T.truthy(a.body:find('<h1 id="title" data-line="0">', 1, true))
    T.truthy(a.body:find(('<base href="/%s/b/%d/asset/">'):format(token, buf), 1, true))
    T.eq(tonumber(a.headers["content-length"]), #a.body)
    local nonce_b = b.headers["content-security-policy"]:match("script%-src 'nonce%-(%x+)'")
    T.truthy(nonce_b ~= nonce, "nonce reused")
    cleanup(buf)
  end)

  T.it("rejects methods other than GET", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = connect(port, ("POST /%s/b/%d HTTP/1.1\r\nHost: 127.0.0.1:%d\r\nContent-Length: 3\r\n\r\nabc"):format(token, buf, port))
    T.truthy(wait(function() return c.eof end))
    shut(c)
    local res = parse(c.data)
    T.eq(res.status, 405)
    T.eq(res.headers.allow, "GET")
    cleanup(buf)
  end)

  T.it("serves images under the file's directory and nothing else", function()
    local buf = fixture()
    local port, token = open(buf)
    local base = ("/%s/b/%d/asset/"):format(token, buf)
    local ok = get(port, base .. "img/p.png")
    T.eq(ok.status, 200)
    T.eq(ok.headers["content-type"], "image/png")
    T.eq(ok.body, "PNGDATA")
    T.truthy(ok.headers["content-security-policy"]:find("sandbox", 1, true))
    T.eq(get(port, base .. "../secret.png").status, 404)
    T.eq(get(port, base .. "%2e%2e/secret.png").status, 404)
    T.eq(get(port, base .. "img/%2e%2e/%2e%2e/secret.png").status, 404)
    T.eq(get(port, base .. "link.png").status, 404)
    T.eq(get(port, base .. "notes.txt").status, 404)
    T.eq(get(port, base .. "missing.png").status, 404)
    T.eq(get(port, base .. "img").status, 404)
    cleanup(buf)
  end)

  T.it("answers 431 to request headers over 16 KiB and closes", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = connect(port, ("GET /%s/b/%d HTTP/1.1\r\nX-Big: %s"):format(token, buf, string.rep("a", 17 * 1024)))
    T.truthy(wait(function() return c.eof end), "connection closed")
    shut(c)
    T.eq(parse(c.data).status, 431)
    cleanup(buf)
  end)

  T.it("assembles a request that arrives in pieces", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = connect(port, "GE")
    T.truthy(wait(function() return c.connected end))
    c.tcp:write(("T /%s/b/%d HTTP/1.1\r\n"):format(token, buf))
    vim.wait(20)
    c.tcp:write("Host: 127.0.0.1:" .. port .. "\r\n\r\n")
    T.truthy(wait(function() return c.eof end))
    shut(c)
    T.eq(parse(c.data).status, 200)
    cleanup(buf)
  end)

  T.it("streams a render on connect, then one debounced render per burst of edits", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = sse(port, token, buf)
    local res = parse(c.data)
    T.eq(res.status, 200)
    T.eq(res.headers["content-type"], "text/event-stream; charset=utf-8")
    T.truthy(events_of(c.data)[1].data.html:find("<p data-line=\"2\">para</p>", 1, true))
    for i = 1, 3 do
      vim.api.nvim_buf_set_lines(buf, 2, 3, false, { "edited " .. i })
      vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
    end
    local t0 = uv.hrtime()
    T.truthy(wait(function() return count(events_of(c.data), "render") >= 2 end), "render after edit")
    local elapsed = (uv.hrtime() - t0) / 1e6
    T.truthy(elapsed >= P.DEBOUNCE - 10, ("render after %.1f ms, before the debounce"):format(elapsed))
    vim.wait(P.DEBOUNCE * 3)
    local list = events_of(c.data)
    T.eq(count(list, "render"), 2)
    T.truthy(list[#list].data.html:find("edited 3", 1, true))
    shut(c)
    cleanup(buf)
  end)

  T.it("sends cursor lines, throttled, and only when the line changes", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = sse(port, token, buf)
    vim.api.nvim_win_set_cursor(0, { 3, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
    vim.api.nvim_win_set_cursor(0, { 5, 0 })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
    T.truthy(wait(function() return count(events_of(c.data), "cursor") >= 2 end), "cursor events")
    vim.wait(P.CURSOR_THROTTLE * 3)
    local lines = {}
    for _, e in ipairs(events_of(c.data)) do
      if e.name == "cursor" then lines[#lines + 1] = e.data.line end
    end
    T.eq(lines, { 2, 4 })
    shut(c)
    cleanup(buf)
  end)

  T.it("survives a client that disconnects mid-stream", function()
    local buf = fixture()
    local port, token = open(buf)
    local notes = {}
    local orig = vim.notify
    vim.notify = function(msg, level) notes[#notes + 1] = { msg, level } end
    local c = sse(port, token, buf)
    shut(c)
    T.truthy(wait(function() return next(P._state().previews[buf].clients) == nil end), "client dropped")
    vim.api.nvim_buf_set_lines(buf, 0, 1, false, { "# Changed" })
    vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
    vim.wait(P.DEBOUNCE * 3)
    vim.notify = orig
    T.eq(notes, {})
    T.eq(get(port, ("/%s/b/%d"):format(token, buf)).status, 200)
    cleanup(buf)
  end)

  T.it("closes a buffer's streams when it is wiped and stops with the last preview", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = sse(port, token, buf)
    vim.api.nvim_buf_delete(buf, { force = true })
    T.truthy(wait(function() return c.eof end), "stream closed")
    shut(c)
    T.eq(P._state().server, nil)
    T.eq(P.is_active(buf), false)
    local probe = connect(port, "GET / HTTP/1.1\r\n\r\n")
    T.truthy(wait(function() return probe.eof end))
    shut(probe)
    T.truthy(probe.err, "server still accepting")
    cleanup(buf)
  end)

  T.it("keeps the preview across :edit! and closes it on :bunload", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = sse(port, token, buf)
    vim.api.nvim_buf_set_lines(buf, 0, 1, false, { "# Unsaved" })
    vim.cmd("edit!")
    T.truthy(wait(function() return count(events_of(c.data), "render") >= 2 end), "render after reload")
    T.eq(P.is_active(buf), true)
    local list = events_of(c.data)
    T.truthy(list[#list].data.html:find('id="title"', 1, true))
    vim.cmd("enew")
    vim.cmd("bunload " .. buf)
    T.truthy(wait(function() return c.eof end), "stream closed on unload")
    shut(c)
    T.eq(P.is_active(buf), false)
    T.eq(P._state().server, nil)
    cleanup(buf)
  end)

  T.it("keeps serving other buffers until the last preview closes", function()
    local a = fixture()
    local port = open(a)
    local b = fixture()
    T.eq(open(b), port)
    P.close(a)
    T.truthy(P._state().server ~= nil)
    P.toggle(b)
    T.eq(P._state().server, nil)
    cleanup(a)
    cleanup(b)
  end)

  T.it("pushes the theme to open streams on ColorScheme", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = sse(port, token, buf)
    vim.api.nvim_exec_autocmds("ColorScheme", {})
    T.truthy(wait(function() return count(events_of(c.data), "theme") >= 1 end), "theme event")
    for _, e in ipairs(events_of(c.data)) do
      if e.name == "theme" then
        T.eq(e.data.dark, vim.o.background == "dark")
        T.eq(type(e.data.css), "string")
      end
    end
    shut(c)
    cleanup(buf)
  end)

  T.it("leaves no timers or handles running after stop", function()
    local buf = fixture()
    local port, token = open(buf)
    local c = sse(port, token, buf)
    vim.api.nvim_exec_autocmds("TextChanged", { buffer = buf })
    vim.api.nvim_exec_autocmds("CursorMoved", { buffer = buf })
    local st = P._state()
    local p = st.previews[buf]
    local srv = st.server
    P.stop()
    T.truthy(wait(function() return c.eof end))
    shut(c)
    T.truthy(srv.tcp:is_closing())
    T.truthy(not p.timer or p.timer:is_closing())
    T.truthy(not p.cursor_timer or p.cursor_timer:is_closing())
    T.eq(next(srv.conns), nil)
    local ok, cmds = pcall(vim.api.nvim_get_autocmds, { group = "LibPreview" })
    T.truthy(not ok or #cmds == 0, "autocmds left behind")
    cleanup(buf)
  end)
end)

local Server = require("lib.preview.server")
local Page = require("lib.preview.page")
local Render = require("lib.preview.render")

local function raw(port, req)
  local c = connect(port, req)
  T.truthy(wait(function() return c.eof end), "response")
  shut(c)
  return parse(c.data) or { raw = c.data }
end

local function vendor_fixture()
  local root = vim.fn.tempname()
  local k = root .. "/katex"
  vim.fn.mkdir(k .. "/dist/fonts", "p")
  vim.fn.writefile({ "window.katex={render:function(){}};" }, k .. "/dist/katex.min.js")
  vim.fn.writefile({ ".katex{}" }, k .. "/dist/katex.min.css")
  vim.fn.writefile({ "WOFF2" }, k .. "/dist/fonts/a.woff2", "b")
  vim.fn.writefile({ "{}" }, k .. "/dist/katex.min.js.map")
  vim.fn.writefile({ "SECRET" }, root .. "/secret.js")
  uv.fs_symlink(root .. "/secret.js", k .. "/dist/link.js")
  local m = root .. "/mermaid"
  vim.fn.mkdir(m .. "/dist", "p")
  vim.fn.writefile({ "window.mermaid={};" }, m .. "/dist/mermaid.min.js")
  local orig = P.vendor_dir
  P.vendor_dir = function(name)
    if name == "katex" then return k end
    if name == "mermaid" then return m end
    return nil
  end
  return function() P.vendor_dir = orig end
end

T.describe("lib.preview hardening", function()
  T.it("sends a CSP with no external origins and a strict-dynamic nonce", function()
    local buf = fixture()
    local port, token = open(buf)
    local res = get(port, ("/%s/b/%d"):format(token, buf))
    local csp = res.headers["content-security-policy"]
    local nonce = csp:match("'nonce%-(%x+)'")
    T.eq(csp, ("default-src 'none'; script-src 'nonce-%s' 'strict-dynamic'; style-src 'self' 'unsafe-inline'; "
      .. "font-src 'self'; img-src 'self' data: https:; connect-src 'self'"):format(nonce))
    T.eq(csp:gsub("img%-src [^;]*", ""):find("https?:"), nil)
    cleanup(buf)
  end)

  T.it("leaves a document's own script tags without the nonce", function()
    local buf = fixture()
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, {
      "<script src=\"/x/vendor/katex/dist/katex.min.js\"></script>", "", "<script>alert(1)</script>",
    })
    local port, token = open(buf)
    local res = get(port, ("/%s/b/%d"):format(token, buf))
    local nonce = res.headers["content-security-policy"]:match("'nonce%-(%x+)'")
    local _, n = res.body:gsub(vim.pesc(nonce), "")
    T.eq(n, 1, "only the page script carries the nonce")
    T.truthy(res.body:find('<script src="/x/vendor/katex/dist/katex.min.js"></script>', 1, true))
    cleanup(buf)
  end)

  T.it("sends Referrer-Policy: no-referrer on every response", function()
    local restore = vendor_fixture()
    local buf = fixture()
    local port, token = open(buf)
    for _, path in ipairs({
      ("/%s/b/%d"):format(token, buf), ("/%s/b/%d/asset/img/p.png"):format(token, buf),
      ("/%s/page.js"):format(token), ("/%s/vendor/katex/dist/katex.min.js"):format(token), "/nope",
    }) do
      T.eq(get(port, path).headers["referrer-policy"], "no-referrer", path)
    end
    local c = sse(port, token, buf)
    T.eq(parse(c.data).headers["referrer-policy"], "no-referrer")
    shut(c)
    restore()
    cleanup(buf)
  end)

  T.it("rejects requests whose Host is not the loopback origin", function()
    local buf = fixture()
    local port, token = open(buf)
    local path = ("/%s/b/%d"):format(token, buf)
    local function with_host(h)
      return raw(port, "GET " .. path .. " HTTP/1.1\r\n" .. (h and ("Host: " .. h .. "\r\n") or "") .. "\r\n").status
    end
    T.eq(with_host("127.0.0.1:" .. port), 200)
    T.eq(with_host("localhost:" .. port), 200)
    T.eq(with_host("LOCALHOST:" .. port), 200)
    T.eq(with_host("evil.example:" .. port), 421)
    T.eq(with_host("127.0.0.1:" .. (port + 1)), 421)
    T.eq(with_host("127.0.0.1"), 421)
    T.eq(with_host(nil), 421)
    cleanup(buf)
  end)

  T.it("answers 400 to a request with more than one Host header", function()
    local buf = fixture()
    local port, token = open(buf)
    local path = ("/%s/b/%d"):format(token, buf)
    local ok = "127.0.0.1:" .. port
    for _, hosts in ipairs({ { ok, ok }, { "evil.example", ok }, { ok, "evil.example" } }) do
      local req = "GET " .. path .. " HTTP/1.1\r\nHost: " .. hosts[1] .. "\r\nhost: " .. hosts[2] .. "\r\n\r\n"
      T.eq(raw(port, req).status, 400, table.concat(hosts, " "))
    end
    cleanup(buf)
  end)

  T.it("parses a request with no headers instead of answering 400", function()
    local buf = fixture()
    local port, token = open(buf)
    T.eq(raw(port, ("GET /%s/b/%d HTTP/1.1\r\n\r\n"):format(token, buf)).status, 421)
    T.eq(raw(port, ("GET /%s/b/%d HTTP/1.1\n\n"):format(token, buf)).status, 421)
    cleanup(buf)
  end)

  T.it("closes connections beyond the cap at once", function()
    local prev = Server.MAX_CONNS
    Server.MAX_CONNS = 3
    local buf = fixture()
    local port, token = open(buf)
    local idle = {}
    for i = 1, 3 do
      idle[i] = connect(port)
      T.truthy(wait(function() return idle[i].connected end))
    end
    T.truthy(wait(function() return P._state().server.nconns == 3 end), "three accepted")
    local extra = connect(port)
    T.truthy(wait(function() return extra.eof end), "extra connection closed")
    T.eq(extra.data, "")
    for _, c in ipairs(idle) do shut(c) end
    T.truthy(wait(function() return P._state().server.nconns == 0 end), "slots released")
    T.eq(get(port, ("/%s/b/%d"):format(token, buf)).status, 200)
    Server.MAX_CONNS = prev
    cleanup(buf)
  end)

  T.it("drops an event stream whose client stops reading", function()
    local prev = Server.MAX_QUEUE
    Server.MAX_QUEUE = 256 * 1024
    local buf = fixture()
    local port, token = open(buf)
    local tcp = uv.new_tcp()
    local connected = false
    tcp:connect("127.0.0.1", port, function()
      connected = true
      tcp:write(("GET /%s/b/%d/events HTTP/1.1\r\nHost: 127.0.0.1:%d\r\n\r\n"):format(token, buf, port))
    end)
    T.truthy(wait(function() return connected and next(P._state().previews[buf].clients) ~= nil end), "stream open")
    local stream = next(P._state().previews[buf].clients)
    local chunk = "data: " .. string.rep("x", 1024 * 1024) .. "\n\n"
    local dropped
    for i = 1, 200 do
      if not stream.write(chunk) then dropped = i break end
      vim.wait(1)
    end
    T.truthy(dropped, "stream was never dropped")
    T.truthy(stream.closed())
    T.truthy(wait(function() return next(P._state().previews[buf].clients) == nil end), "client removed")
    tcp:close()
    Server.MAX_QUEUE = prev
    cleanup(buf)
  end)

  T.it("puts the page script in head so an unclosed comment cannot swallow it", function()
    local buf = fixture()
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, { "# T", "", "<!--" })
    local port, token = open(buf)
    local body = get(port, ("/%s/b/%d"):format(token, buf)).body
    local head_end = body:find("</head>", 1, true)
    local cfg = body:match('<script type="application/json" id="mdp%-cfg">(.-)</script>')
    T.eq(vim.json.decode(cfg).events, ("/%s/b/%d/events"):format(token, buf))
    T.truthy(body:find('id="mdp-cfg"', 1, true) < head_end)
    T.truthy(body:find("page.js", 1, true) < head_end)
    T.truthy(body:find("<!--", head_end, true))
    T.truthy(Page.JS:find('document.getElementById("mdp-cfg")', 1, true))
    cleanup(buf)
  end)

  T.it("serves vendor files only from inside the plugin dir, with their types", function()
    local restore = vendor_fixture()
    local buf = fixture()
    local port, token = open(buf)
    local v = ("/%s/vendor/"):format(token)
    local js = get(port, v .. "katex/dist/katex.min.js")
    T.eq(js.status, 200)
    T.eq(js.headers["content-type"], "text/javascript; charset=utf-8")
    T.eq(js.body, "window.katex={render:function(){}};\n")
    T.eq(get(port, v .. "katex/dist/katex.min.css").headers["content-type"], "text/css; charset=utf-8")
    T.eq(get(port, v .. "katex/dist/fonts/a.woff2").headers["content-type"], "font/woff2")
    T.eq(get(port, v .. "mermaid/dist/mermaid.min.js").status, 200)
    for _, bad in ipairs({
      "katex/../secret.js", "katex/dist/%2e%2e/%2e%2e/secret.js", "katex/%2e%2e/secret.js", "katex/dist/link.js",
      "katex/dist/katex.min.js.map", "katex/dist", "katex/dist/missing.js", "other/dist/katex.min.js",
      "..%2fsecret.js", "katex/dist/katex.min.js%00.css",
    }) do
      T.eq(get(port, v .. bad).status, 404, bad)
    end
    T.eq(get(port, ("/%s/vendor/katex/dist/katex.min.js"):format(string.rep("0", 32))).status, 404)
    local page = get(port, ("/%s/b/%d"):format(token, buf)).body
    local cfg = vim.json.decode(page:match('id="mdp%-cfg">(.-)</script>'))
    T.eq(cfg.katex, { js = v .. "katex/dist/katex.min.js", css = v .. "katex/dist/katex.min.css" })
    T.eq(cfg.mermaid, { js = v .. "mermaid/dist/mermaid.min.js" })
    T.eq(page:find("https?://[%w%.]*jsdelivr"), nil)
    restore()
    cleanup(buf)
  end)

  T.it("leaves math and diagrams as source and says :Pack install once when vendor files are missing", function()
    local orig = P.vendor_dir
    P.vendor_dir = function() return nil end
    P._state().vendor_checked = nil
    local notes = {}
    local orig_notify = vim.notify
    vim.notify = function(msg) notes[#notes + 1] = msg end
    local buf = fixture()
    local port, token = open(buf)
    local cfg = vim.json.decode(get(port, ("/%s/b/%d"):format(token, buf)).body:match('id="mdp%-cfg">(.-)</script>'))
    P.close(buf)
    open(buf)
    vim.notify = orig_notify
    P.vendor_dir = orig
    T.eq(cfg.katex, vim.NIL)
    T.eq(cfg.mermaid, vim.NIL)
    T.eq(#notes, 1)
    T.truthy(notes[1]:find(":Pack install", 1, true), notes[1])
    cleanup(buf)
  end)

  T.it("reuses the last render while the buffer is unchanged and stretches the debounce for slow renders", function()
    local buf = fixture()
    local port, token = open(buf)
    local orig = Render.render
    local calls = 0
    Render.render = function(...) calls = calls + 1; return orig(...) end
    local path = ("/%s/b/%d"):format(token, buf)
    get(port, path)
    get(port, path)
    T.eq(calls, 1)
    vim.api.nvim_buf_set_lines(buf, 0, 1, false, { "# Changed" })
    T.truthy(get(port, path).body:find("Changed", 1, true))
    T.eq(calls, 2)
    Render.render = orig
    T.eq(P.debounce_ms({ render_ms = 5 }), P.DEBOUNCE)
    T.eq(P.debounce_ms({ render_ms = 400 }), 800)
    cleanup(buf)
  end)
end)
