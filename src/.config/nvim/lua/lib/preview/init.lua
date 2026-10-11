local Render = require("lib.preview.render")
local Page = require("lib.preview.page")
local Server = require("lib.preview.server")

local uv = vim.uv

local M = {}

M.DEBOUNCE = 60
M.CURSOR_THROTTLE = 30
M.HEARTBEAT = 20000

local IMAGE_TYPES = {
  png = "image/png",
  jpg = "image/jpeg",
  jpeg = "image/jpeg",
  gif = "image/gif",
  svg = "image/svg+xml",
  webp = "image/webp",
  avif = "image/avif",
  ico = "image/x-icon",
  bmp = "image/bmp",
}

local VENDOR_TYPES = {
  js = "text/javascript; charset=utf-8",
  css = "text/css; charset=utf-8",
  woff2 = "font/woff2",
  woff = "font/woff",
  ttf = "font/ttf",
}

local state = { previews = {} }

local function hex(bytes)
  return (bytes:gsub(".", function(c) return string.format("%02x", c:byte()) end))
end

local function random_hex()
  return hex(assert(uv.random(16)))
end

local function close_timer(t)
  if t and not t:is_closing() then
    t:stop()
    t:close()
  end
end

local function event(name, payload)
  return "event: " .. name .. "\ndata: " .. vim.json.encode(payload) .. "\n\n"
end

local function broadcast(p, data)
  for stream in pairs(p.clients) do
    if not stream.write(data) then p.clients[stream] = nil end
  end
end

local function each_preview(fn)
  for buf, p in pairs(state.previews) do fn(buf, p) end
end

local function resolve_buf(buf)
  if buf == nil or buf == 0 then return vim.api.nvim_get_current_buf() end
  return buf
end

function M.url(buf)
  buf = resolve_buf(buf)
  if not (state.server and state.previews[buf]) then return nil end
  return ("http://127.0.0.1:%d/%s/b/%d"):format(state.server.port, state.token, buf)
end

function M.is_active(buf)
  return state.previews[resolve_buf(buf)] ~= nil
end

function M.open_url(url)
  return vim.ui.open(url)
end

local function rendered(buf, p)
  local key = vim.api.nvim_buf_get_changedtick(buf) .. "\0" .. vim.bo[buf].filetype
  if p.html and p.html_key == key then return p.html end
  local t0 = uv.hrtime()
  p.html = Render.render(buf, p.cache)
  p.html_key = key
  p.render_ms = (uv.hrtime() - t0) / 1e6
  return p.html
end

function M.debounce_ms(p)
  return math.max(M.DEBOUNCE, math.ceil(2 * (p and p.render_ms or 0)))
end

local function flush(buf)
  local p = state.previews[buf]
  if not p or next(p.clients) == nil or not vim.api.nvim_buf_is_valid(buf) then return end
  broadcast(p, event("render", { html = rendered(buf, p) }))
end

local function schedule_render(buf)
  local p = state.previews[buf]
  if not p or next(p.clients) == nil then return end
  p.timer = p.timer or uv.new_timer()
  p.timer:stop()
  p.timer:start(M.debounce_ms(p), 0, vim.schedule_wrap(function() flush(buf) end))
end

local function send_cursor(p, line)
  if line == p.sent_line then return end
  p.sent_line = line
  p.sent_at = uv.now()
  broadcast(p, event("cursor", { line = line }))
end

local function on_cursor(buf)
  local p = state.previews[buf]
  if not p or next(p.clients) == nil then return end
  local line = vim.api.nvim_win_get_cursor(0)[1] - 1
  local wait = M.CURSOR_THROTTLE - (uv.now() - (p.sent_at or 0))
  if wait <= 0 and not (p.cursor_timer and p.cursor_timer:is_active()) then
    return send_cursor(p, line)
  end
  p.pending_line = line
  p.cursor_timer = p.cursor_timer or uv.new_timer()
  if p.cursor_timer:is_active() then return end
  p.cursor_timer:start(math.max(wait, 1), 0, vim.schedule_wrap(function()
    local cur = state.previews[buf]
    if cur and cur.pending_line then
      local l = cur.pending_line
      cur.pending_line = nil
      send_cursor(cur, l)
    end
  end))
end

local function read_file(path, cb)
  uv.fs_open(path, "r", 438, function(oerr, fd)
    if oerr or not fd then return vim.schedule(function() cb(nil) end) end
    uv.fs_fstat(fd, function(serr, stat)
      if serr or not stat or stat.type ~= "file" then
        uv.fs_close(fd)
        return vim.schedule(function() cb(nil) end)
      end
      uv.fs_read(fd, stat.size, 0, function(rerr, data)
        uv.fs_close(fd)
        vim.schedule(function() cb((not rerr and data and #data == stat.size) and data or nil) end)
      end)
    end)
  end)
end

local function inside(base, rel)
  if not base or rel == "" or rel:find("%z") then return nil end
  local full = uv.fs_realpath(base .. "/" .. rel)
  if not full or full:sub(1, #base + 1) ~= base .. "/" then return nil end
  return full
end

local function send_file(res, full, headers)
  read_file(full, function(body)
    if not body then return res.fail(404) end
    res.send(200, headers, body)
  end)
end

local function asset(buf, rel, res)
  local name = vim.api.nvim_buf_get_name(buf)
  if name == "" then return res.fail(404) end
  local full = inside(uv.fs_realpath(vim.fs.dirname(name)), rel)
  local ctype = full and IMAGE_TYPES[(full:match("%.([^./]+)$") or ""):lower()]
  if not ctype then return res.fail(404) end
  send_file(res, full, {
    { "Content-Type", ctype },
    { "Content-Security-Policy", "default-src 'none'; style-src 'unsafe-inline'; sandbox" },
    { "X-Content-Type-Options", "nosniff" },
    { "Cache-Control", "no-store" },
  })
end

function M.vendor_dir(name)
  if not Page.VENDOR[name] then return nil end
  local ok, dir = pcall(function() return Lib.plugin.dir(name) end)
  return ok and dir or nil
end

local function vendor(name, rel, res)
  local dir = M.vendor_dir(name)
  local full = dir and inside(uv.fs_realpath(dir), rel)
  local ctype = full and VENDOR_TYPES[(full:match("%.([^./]+)$") or ""):lower()]
  if not ctype then return res.fail(404) end
  send_file(res, full, {
    { "Content-Type", ctype },
    { "X-Content-Type-Options", "nosniff" },
    { "Cache-Control", "private, max-age=300" },
  })
end

local function vendor_cfg()
  local out = {}
  for name, files in pairs(Page.VENDOR) do
    local dir = M.vendor_dir(name)
    if dir then
      local entry = {}
      for kind, rel in pairs(files) do
        if uv.fs_stat(dir .. "/" .. rel) then entry[kind] = ("/%s/vendor/%s/%s"):format(state.token, name, rel) end
      end
      if entry.js then out[name] = entry end
    end
  end
  return out
end

local function script(res)
  res.send(200, {
    { "Content-Type", "text/javascript; charset=utf-8" },
    { "X-Content-Type-Options", "nosniff" },
    { "Cache-Control", "no-store" },
  }, Page.JS)
end

local function page(buf, res)
  local p = state.previews[buf]
  local nonce = random_hex()
  local name = vim.api.nvim_buf_get_name(buf)
  local prefix = ("/%s/b/%d"):format(state.token, buf)
  local html = Page.html({
    title = name ~= "" and vim.fs.basename(name) or "[No Name]",
    nonce = nonce,
    events = prefix .. "/events",
    base = prefix .. "/asset/",
    script = "/" .. state.token .. "/page.js",
    vendor = vendor_cfg(),
    theme = Page.theme(),
    content = rendered(buf, p),
    escape = Render.escape,
  })
  res.send(200, {
    { "Content-Type", "text/html; charset=utf-8" },
    { "Content-Security-Policy", Page.csp(nonce) },
    { "X-Content-Type-Options", "nosniff" },
    { "Cache-Control", "no-store" },
  }, html)
end

local function events(buf, res)
  local p = state.previews[buf]
  local stream = res.stream({
    { "Content-Type", "text/event-stream; charset=utf-8" },
    { "Cache-Control", "no-store" },
    { "X-Content-Type-Options", "nosniff" },
  })
  p.clients[stream] = true
  stream.on_close(function()
    local cur = state.previews[buf]
    if cur then cur.clients[stream] = nil end
  end)
  stream.write(event("render", { html = rendered(buf, p) }))
end

local function handle(req, res)
  local vtoken, vname, vrel = req.decoded:match("^/(%x+)/vendor/([%w%-%._]+)/(.+)$")
  if vtoken and vtoken == state.token then return vendor(vname, vrel, res) end
  if req.decoded == "/" .. tostring(state.token) .. "/page.js" then return script(res) end
  local token, buf, rest = req.decoded:match("^/(%x+)/b/(%d+)(.*)$")
  buf = tonumber(buf)
  if token ~= state.token or not buf or not state.previews[buf] or not vim.api.nvim_buf_is_valid(buf) then
    return res.fail(404)
  end
  if rest == "" then return page(buf, res) end
  if rest == "/events" then return events(buf, res) end
  local rel = rest:match("^/asset/(.+)$")
  if rel then return asset(buf, rel, res) end
  res.fail(404)
end

local function send_theme()
  local theme = Page.theme()
  local data = event("theme", theme)
  each_preview(function(_, p) broadcast(p, data) end)
end

local function ensure_server()
  if state.server then return true end
  local srv, err = Server.start({ host = "127.0.0.1", port = vim.g.markdown_preview_port or 0, handler = handle })
  if not srv then
    vim.notify("markdown preview: cannot listen: " .. tostring(err), vim.log.levels.ERROR)
    return false
  end
  state.server = srv
  state.token = random_hex()
  state.group = vim.api.nvim_create_augroup("LibPreview", { clear = true })
  vim.api.nvim_create_autocmd("VimLeavePre", { group = state.group, callback = function() M.stop() end })
  vim.api.nvim_create_autocmd("ColorScheme", { group = state.group, callback = send_theme })
  vim.api.nvim_create_autocmd("OptionSet", { group = state.group, pattern = "background", callback = send_theme })
  state.heartbeat = uv.new_timer()
  state.heartbeat:start(M.HEARTBEAT, M.HEARTBEAT, function()
    each_preview(function(_, p) broadcast(p, ":\n\n") end)
  end)
  return true
end

local function check_vendor()
  if state.vendor_checked then return end
  state.vendor_checked = true
  local missing = {}
  for name in pairs(Page.VENDOR) do
    if not M.vendor_dir(name) then missing[#missing + 1] = name end
  end
  if #missing == 0 then return end
  table.sort(missing)
  vim.notify(("markdown preview: math and diagrams show as source until %s %s installed (:Pack install)")
    :format(table.concat(missing, " and "), #missing == 1 and "is" or "are"), vim.log.levels.INFO)
end

function M.open(buf)
  buf = resolve_buf(buf)
  if not vim.api.nvim_buf_is_valid(buf) then return nil end
  check_vendor()
  if not state.previews[buf] then
    if not ensure_server() then return nil end
    state.previews[buf] = { clients = {}, cache = {} }
    local group = state.group
    vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI", "BufWritePost" }, {
      group = group,
      buffer = buf,
      callback = function() schedule_render(buf) end,
    })
    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
      group = group,
      buffer = buf,
      callback = function() on_cursor(buf) end,
    })
    vim.api.nvim_create_autocmd("BufWipeout", {
      group = group,
      buffer = buf,
      callback = function() M.close(buf) end,
    })
    vim.api.nvim_create_autocmd("BufUnload", {
      group = group,
      buffer = buf,
      callback = function()
        vim.schedule(function()
          if not state.previews[buf] then return end
          if vim.api.nvim_buf_is_loaded(buf) then return schedule_render(buf) end
          M.close(buf)
        end)
      end,
    })
  end
  local url = M.url(buf)
  M.open_url(url)
  return url
end

local function release(buf, p)
  for stream in pairs(p.clients) do stream.close() end
  p.clients = {}
  close_timer(p.timer)
  close_timer(p.cursor_timer)
  if state.group then pcall(vim.api.nvim_clear_autocmds, { group = state.group, buffer = buf }) end
end

function M.close(buf)
  buf = resolve_buf(buf)
  local p = state.previews[buf]
  if not p then return end
  state.previews[buf] = nil
  release(buf, p)
  if next(state.previews) == nil then M.stop() end
end

function M.toggle(buf)
  buf = resolve_buf(buf)
  if state.previews[buf] then return M.close(buf) end
  return M.open(buf)
end

function M.stop()
  local previews = state.previews
  state.previews = {}
  for buf, p in pairs(previews) do release(buf, p) end
  close_timer(state.heartbeat)
  if state.server then state.server:stop() end
  if state.group then pcall(vim.api.nvim_del_augroup_by_id, state.group) end
  state.server, state.token, state.group, state.heartbeat = nil, nil, nil, nil
end

function M._state()
  return state
end

return M
