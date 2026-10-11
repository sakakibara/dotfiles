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
  local c = connect(port, "GET " .. path .. " HTTP/1.1\r\nHost: 127.0.0.1\r\n" .. (extra or "") .. "\r\n")
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
  local c = connect(port, ("GET /%s/b/%d/events HTTP/1.1\r\nHost: x\r\nAccept: text/event-stream\r\n\r\n"):format(token, buf))
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
    T.truthy(a.body:find('<script nonce="' .. nonce .. '">', 1, true))
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
    local c = connect(port, ("POST /%s/b/%d HTTP/1.1\r\nHost: x\r\nContent-Length: 3\r\n\r\nabc"):format(token, buf))
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
    c.tcp:write("Host: x\r\n\r\n")
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
