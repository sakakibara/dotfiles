local uv = vim.uv

local M = {}

M.MAX_HEADER = 16 * 1024
M.HEADER_TIMEOUT = 10000
M.MAX_CONNS = 64
M.MAX_QUEUE = 8 * 1024 * 1024

local REASONS = {
  [200] = "OK",
  [400] = "Bad Request",
  [404] = "Not Found",
  [405] = "Method Not Allowed",
  [421] = "Misdirected Request",
  [431] = "Request Header Fields Too Large",
  [500] = "Internal Server Error",
}

local Server = {}
Server.__index = Server

function Server:warn(msg)
  if self.warned then return end
  self.warned = true
  vim.schedule(function()
    vim.notify("markdown preview: " .. msg, vim.log.levels.WARN)
  end)
end

function Server:drop(conn)
  if conn.closed then return end
  conn.closed = true
  if self.conns[conn] then self.nconns = self.nconns - 1 end
  self.conns[conn] = nil
  if conn.timer then
    pcall(conn.timer.stop, conn.timer)
    if not conn.timer:is_closing() then conn.timer:close() end
    conn.timer = nil
  end
  if not conn.tcp:is_closing() then
    pcall(conn.tcp.read_stop, conn.tcp)
    conn.tcp:close()
  end
  local cb = conn.on_close
  conn.on_close = nil
  if cb then
    local ok, err = pcall(cb)
    if not ok then self:warn(tostring(err)) end
  end
end

function Server:write(conn, data, after)
  if conn.closed then return false end
  local ok, res = pcall(conn.tcp.write, conn.tcp, data, function(err)
    if err then
      self:drop(conn)
    elseif after then
      after()
    end
  end)
  if not ok or not res then
    self:drop(conn)
    return false
  end
  return true
end

local function head(status, headers)
  local out = { "HTTP/1.1 " .. status .. " " .. (REASONS[status] or "Unknown") }
  for _, h in ipairs(headers or {}) do out[#out + 1] = h[1] .. ": " .. h[2] end
  out[#out + 1] = "Referrer-Policy: no-referrer"
  out[#out + 1] = ""
  out[#out + 1] = ""
  return table.concat(out, "\r\n")
end

function Server:respond(conn, status, headers, body)
  body = body or ""
  local hs = vim.list_extend({}, headers or {})
  hs[#hs + 1] = { "Content-Length", tostring(#body) }
  hs[#hs + 1] = { "Connection", "close" }
  self:write(conn, head(status, hs) .. body, function()
    if conn.closed or conn.tcp:is_closing() then return end
    local ok = pcall(conn.tcp.shutdown, conn.tcp, function() self:drop(conn) end)
    if not ok then self:drop(conn) end
  end)
end

local function plain(status)
  return { { "Content-Type", "text/plain; charset=utf-8" }, { "Cache-Control", "no-store" } },
    (REASONS[status] or "Error") .. "\n"
end

function Server:fail(conn, status)
  local hs, body = plain(status)
  if status == 405 then hs[#hs + 1] = { "Allow", "GET" } end
  self:respond(conn, status, hs, body)
end

local function decode(s)
  return (s:gsub("%%(%x%x)", function(h) return string.char(tonumber(h, 16)) end))
end

local function parse(raw)
  local method, target = raw:match("^(%u+) (%S+) HTTP/1%.%d\r?\n")
  if not method then return nil end
  local headers = {}
  for name, value in raw:gmatch("\n([%w%-]+):[ \t]*([^\r\n]*)") do
    name = name:lower()
    if name == "host" and headers.host ~= nil then return nil end
    headers[name] = value
  end
  local path = target:match("^[^?#]*")
  return { method = method, target = target, path = path, decoded = decode(path), headers = headers }
end

function Server:dispatch(conn, req)
  local res = {}
  function res.send(status, headers, body)
    self:respond(conn, status, headers, body)
  end
  function res.fail(status)
    self:fail(conn, status)
  end
  function res.stream(headers)
    local hs = vim.list_extend({}, headers or {})
    hs[#hs + 1] = { "Connection", "keep-alive" }
    local stream = {}
    function stream.write(data)
      if conn.closed then return false end
      if conn.tcp:get_write_queue_size() > M.MAX_QUEUE then
        self:drop(conn)
        return false
      end
      return self:write(conn, data)
    end
    function stream.close() self:drop(conn) end
    function stream.closed() return conn.closed end
    function stream.on_close(cb) conn.on_close = cb end
    conn.streaming = true
    self:write(conn, head(200, hs))
    return stream
  end
  local ok, err = pcall(self.handler, req, res)
  if not ok then
    self:warn(tostring(err))
    if not conn.closed and not conn.streaming then self:fail(conn, 500) end
  end
end

function Server:on_data(conn, chunk)
  if conn.parsed then return end
  conn.buf = conn.buf .. chunk
  local stop, stop_end = conn.buf:find("\r?\n\r?\n")
  if not stop then
    if #conn.buf > M.MAX_HEADER then
      conn.parsed = true
      self:fail(conn, 431)
    end
    return
  end
  conn.parsed = true
  if stop > M.MAX_HEADER then return self:fail(conn, 431) end
  if conn.timer then
    conn.timer:stop()
    conn.timer:close()
    conn.timer = nil
  end
  local req = parse(conn.buf:sub(1, stop_end))
  conn.buf = ""
  if not req then return self:fail(conn, 400) end
  if not self.hosts[(req.headers.host or ""):lower()] then return self:fail(conn, 421) end
  if req.method ~= "GET" then return self:fail(conn, 405) end
  vim.schedule(function()
    if not conn.closed then self:dispatch(conn, req) end
  end)
end

function Server:accept()
  local tcp = uv.new_tcp()
  if not tcp then return end
  local ok = pcall(self.tcp.accept, self.tcp, tcp)
  if not ok then
    tcp:close()
    return
  end
  if self.nconns >= M.MAX_CONNS then
    tcp:close()
    return
  end
  local conn = { tcp = tcp, buf = "" }
  self.conns[conn] = true
  self.nconns = self.nconns + 1
  conn.timer = uv.new_timer()
  conn.timer:start(M.HEADER_TIMEOUT, 0, function()
    vim.schedule(function()
      if not conn.parsed then self:drop(conn) end
    end)
  end)
  tcp:read_start(function(err, chunk)
    local okr, e = pcall(function()
      if err or not chunk then
        self:drop(conn)
      else
        self:on_data(conn, chunk)
      end
    end)
    if not okr then
      self:warn(tostring(e))
      self:drop(conn)
    end
  end)
end

function Server:stop()
  if self.stopped then return end
  self.stopped = true
  for conn in pairs(self.conns) do self:drop(conn) end
  if not self.tcp:is_closing() then self.tcp:close() end
end

function M.start(opts)
  local tcp = uv.new_tcp()
  local ok, err = tcp:bind(opts.host or "127.0.0.1", opts.port or 0)
  if not ok then
    tcp:close()
    return nil, err
  end
  local self = setmetatable({ tcp = tcp, conns = {}, nconns = 0, handler = opts.handler }, Server)
  local lok, lerr = tcp:listen(128, function(e)
    if e then return self:warn(e) end
    local aok, aerr = pcall(self.accept, self)
    if not aok then self:warn(tostring(aerr)) end
  end)
  if not lok then
    tcp:close()
    return nil, lerr
  end
  self.port = tcp:getsockname().port
  self.hosts = { ["127.0.0.1:" .. self.port] = true, ["localhost:" .. self.port] = true }
  return self
end

return M
