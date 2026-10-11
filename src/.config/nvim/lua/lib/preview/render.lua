local Slug = require("lib.preview.slug")

local M = {}

local ESC = { ["&"] = "&amp;", ["<"] = "&lt;", [">"] = "&gt;", ['"'] = "&quot;", ["'"] = "&#39;" }

local function esc(s)
  return (s:gsub("[&<>\"']", ESC))
end
M.escape = esc

local function utf8_char(cp)
  if cp < 0x80 then return string.char(cp) end
  if cp < 0x800 then
    return string.char(0xC0 + math.floor(cp / 0x40), 0x80 + cp % 0x40)
  end
  if cp < 0x10000 then
    return string.char(0xE0 + math.floor(cp / 0x1000), 0x80 + math.floor(cp / 0x40) % 0x40, 0x80 + cp % 0x40)
  end
  if cp < 0x110000 then
    return string.char(
      0xF0 + math.floor(cp / 0x40000),
      0x80 + math.floor(cp / 0x1000) % 0x40,
      0x80 + math.floor(cp / 0x40) % 0x40,
      0x80 + cp % 0x40
    )
  end
  return "\239\191\189"
end

local NAMED = { amp = "&", lt = "<", gt = ">", quot = '"', apos = "'", nbsp = "\194\160" }

local function decode_entities(s)
  return (s:gsub("&(#?[xX]?)(%w+);", function(kind, body)
    if kind == "" then return NAMED[body] end
    local cp = kind == "#" and tonumber(body) or tonumber(body, 16)
    if not cp or cp == 0 then return nil end
    return utf8_char(cp)
  end))
end

local function unescape(s)
  return decode_entities((s:gsub("\\([!-/:-@%[-`{-~])", "%1")))
end

local function plain(html)
  return decode_entities((html:gsub("<[^>]*>", "")))
end

local UNSAFE = { javascript = true, vbscript = true }

local function url_attr(u)
  local probe = u:gsub("[%c%s]", ""):lower()
  local scheme = probe:match("^([%a][%w+.%-]*):")
  if scheme and (UNSAFE[scheme] or (scheme == "data" and not probe:match("^data:image/"))) then
    return "#"
  end
  local enc = u:gsub("[^%w%-._~:/?#%[%]@!$&'()*+,;=%%]", function(c)
    return string.format("%%%02X", c:byte())
  end)
  return esc(enc)
end

local function ci(word)
  return (word:gsub("%a", function(c) return "[" .. c:lower() .. c:upper() .. "]" end))
end

local META = "<(" .. ci("meta") .. ")([%s/>])"
local META_END = "<(" .. ci("meta") .. ")$"
local REFPOL = "([%s\"'/])(" .. ci("referrerpolicy") .. ")%f[^%w%-]"

local function passthrough(html)
  html = html:gsub(META, "&lt;%1%2"):gsub(META_END, "&lt;%1")
  html = html:gsub(REFPOL, '%1referrerpolicy="no-referrer" data-mdp-%2')
  return html
end
M.passthrough = passthrough

local function norm_label(s)
  return (s:gsub("%s+", " "):match("^ ?(.-) ?$"):lower())
end

local function srow(node)
  return (node:start())
end

local function sbyte(node)
  local _, _, b = node:start()
  return b
end

local function ebyte(node)
  local _, _, b = node:end_()
  return b
end

local function last_row(node)
  local er, ec = node:end_()
  if ec == 0 and er > srow(node) then return er - 1 end
  return er
end

local function dl(node)
  return ' data-line="' .. srow(node) .. '"'
end

local function slice(C, a, b)
  if a >= b then return "" end
  return C.s:sub(a + 1, b)
end

local function block_text(C, node)
  local a, b = sbyte(node), ebyte(node)
  local parts, pos = {}, a
  for _, child in ipairs(node:named_children()) do
    if child:type() == "block_continuation" then
      local cs, ce = sbyte(child), ebyte(child)
      if cs > pos then parts[#parts + 1] = C.src:sub(pos + 1, cs) end
      pos = ce
    end
  end
  if b > pos then parts[#parts + 1] = C.src:sub(pos + 1, b) end
  return table.concat(parts)
end

local function trim_url(url)
  while true do
    local last = url:sub(-1)
    if last:match("[%?!%.,:%*_~'\"]") then
      url = url:sub(1, -2)
    elseif last == ")" then
      local _, open = url:gsub("%(", "")
      local _, close = url:gsub("%)", "")
      if close > open then url = url:sub(1, -2) else return url end
    elseif last == ";" and url:match("&%w+;$") then
      url = url:gsub("&%w+;$", "")
    else
      return url
    end
  end
end

local function text(C, s)
  if s == "" then return "" end
  if C.in_link or not s:find("https?://") then return esc(s) end
  local out, pos = {}, 1
  while true do
    local a, b = s:find("https?://[%w_%-][^%s<]*", pos)
    if not a then break end
    if a > 1 and s:sub(a - 1, a - 1):match("%w") then
      out[#out + 1] = esc(s:sub(pos, b))
      pos = b + 1
    else
      local url = trim_url(s:sub(a, b))
      out[#out + 1] = esc(s:sub(pos, a - 1))
      out[#out + 1] = '<a href="' .. url_attr(url) .. '">' .. esc(url) .. "</a>"
      pos = a + #url
    end
  end
  out[#out + 1] = esc(s:sub(pos))
  return table.concat(out)
end

local function delim_bounds(node, dtype)
  local a, b = sbyte(node), ebyte(node)
  local n = node:named_child_count()
  local i = 0
  while i < n do
    local c = node:named_child(i)
    if c:type() ~= dtype or sbyte(c) ~= a then break end
    a = ebyte(c)
    i = i + 1
  end
  local j = n - 1
  while j >= i do
    local c = node:named_child(j)
    if c:type() ~= dtype or ebyte(c) ~= b then break end
    b = sbyte(c)
    j = j - 1
  end
  if b < a then b = a end
  return a, b
end

local inline_node

local MATH_CLOSE = { ["\\("] = "\\)", ["\\["] = "\\]" }

local function children_html(C, node, a, b)
  local kids = {}
  for _, child in ipairs(node:named_children()) do
    if sbyte(child) >= a and ebyte(child) <= b then kids[#kids + 1] = child end
  end
  local out, pos, i = {}, a, 1
  while i <= #kids do
    local child = kids[i]
    local cs, ce = sbyte(child), ebyte(child)
    local close = child:type() == "backslash_escape" and not C.in_link and MATH_CLOSE[slice(C, cs, ce)]
    local j
    if close then
      for k = i + 1, #kids do
        if kids[k]:type() == "backslash_escape" and slice(C, sbyte(kids[k]), ebyte(kids[k])) == close then
          j = k
          break
        end
      end
    end
    out[#out + 1] = text(C, slice(C, pos, cs))
    if j then
      local cls = close == "\\)" and "math-inline" or "math-display"
      out[#out + 1] = '<span class="' .. cls .. '">' .. esc(slice(C, ce, sbyte(kids[j]))) .. "</span>"
      pos = ebyte(kids[j])
      i = j + 1
    else
      out[#out + 1] = inline_node(C, child)
      pos = ce
      i = i + 1
    end
  end
  out[#out + 1] = text(C, slice(C, pos, b))
  return table.concat(out)
end

local function wrap(C, node, dtype, open, close)
  local a, b = delim_bounds(node, dtype)
  return open .. children_html(C, node, a, b) .. close
end

local function container(C, node)
  return children_html(C, node, sbyte(node), ebyte(node))
end

local function link_content(C, node)
  if not node then return "" end
  local saved = C.in_link
  C.in_link = true
  local html = children_html(C, node, sbyte(node), ebyte(node))
  C.in_link = saved
  return html
end

local function anchor(C, href, title, content_node)
  local t = title and (' title="' .. esc(title) .. '"') or ""
  return '<a href="' .. url_attr(href) .. '"' .. t .. ">" .. link_content(C, content_node) .. "</a>"
end

local function parts_of(C, node)
  local p = {}
  for _, child in ipairs(node:named_children()) do
    local t = child:type()
    if t == "link_text" or t == "image_description" then
      p.text = child
    elseif t == "link_destination" then
      local d = slice(C, sbyte(child), ebyte(child))
      if d:sub(1, 1) == "<" and d:sub(-1) == ">" then d = d:sub(2, -2) end
      p.dest = unescape(d)
    elseif t == "link_title" then
      p.title = unescape(slice(C, sbyte(child), ebyte(child)):sub(2, -2))
    elseif t == "link_label" then
      p.label = slice(C, sbyte(child) + 1, ebyte(child) - 1)
    end
  end
  return p
end

local function raw_of(C, node)
  return node and slice(C, sbyte(node), ebyte(node)) or ""
end

local function footnote_ref(C, raw)
  if raw:sub(1, 1) ~= "^" then return nil end
  local key = norm_label(raw:sub(2))
  if not C.fn_defs[key] then return nil end
  local n = C.fn_num[key]
  if not n then
    n = #C.fn_order + 1
    C.fn_order[n] = key
    C.fn_num[key] = n
  end
  local k = (C.fn_refs[key] or 0) + 1
  C.fn_refs[key] = k
  local id = "fnref-" .. n .. (k > 1 and ("-" .. k) or "")
  return '<sup><a href="#fn-' .. n .. '" id="' .. id .. '" data-footnote-ref>' .. n .. "</a></sup>"
end

local function ref_link(C, node, label, p)
  local def = C.refs[norm_label(label)]
  if not def then return container(C, node) end
  return anchor(C, def.dest, def.title, p.text)
end

local INLINE = {}

INLINE.emphasis = function(C, node) return wrap(C, node, "emphasis_delimiter", "<em>", "</em>") end
INLINE.strong_emphasis = function(C, node) return wrap(C, node, "emphasis_delimiter", "<strong>", "</strong>") end
INLINE.strikethrough = function(C, node)
  local parent = node:parent()
  if parent and parent:type() == "strikethrough" then
    return wrap(C, node, "emphasis_delimiter", "", "")
  end
  return wrap(C, node, "emphasis_delimiter", "<del>", "</del>")
end

INLINE.code_span = function(C, node)
  local a, b = delim_bounds(node, "code_span_delimiter")
  local s = slice(C, a, b):gsub("\r?\n", " ")
  if C.in_table then s = s:gsub("\\|", "|") end
  if s:match("^ .* $") and s:find("[^ ]") then s = s:sub(2, -2) end
  return "<code>" .. esc(s) .. "</code>"
end

INLINE.latex_block = function(C, node)
  local a, b = delim_bounds(node, "latex_span_delimiter")
  local display = a - sbyte(node) >= 2
  local cls = display and "math-display" or "math-inline"
  return '<span class="' .. cls .. '">' .. esc(slice(C, a, b)) .. "</span>"
end

INLINE.backslash_escape = function(C, node)
  return esc(slice(C, sbyte(node) + 1, ebyte(node)))
end

INLINE.hard_line_break = function() return "<br>\n" end

local function raw_inline(C, node) return slice(C, sbyte(node), ebyte(node)) end
INLINE.html_tag = function(C, node) return passthrough(raw_inline(C, node)) end
INLINE.entity_reference = raw_inline
INLINE.numeric_character_reference = raw_inline

INLINE.uri_autolink = function(C, node)
  local s = slice(C, sbyte(node) + 1, ebyte(node) - 1)
  return '<a href="' .. url_attr(s) .. '">' .. esc(s) .. "</a>"
end

INLINE.email_autolink = function(C, node)
  local s = slice(C, sbyte(node) + 1, ebyte(node) - 1)
  return '<a href="' .. url_attr("mailto:" .. s) .. '">' .. esc(s) .. "</a>"
end

INLINE.inline_link = function(C, node)
  local p = parts_of(C, node)
  return anchor(C, p.dest or "", p.title, p.text)
end

INLINE.full_reference_link = function(C, node)
  local p = parts_of(C, node)
  return ref_link(C, node, p.label or "", p)
end

INLINE.collapsed_reference_link = function(C, node)
  local p = parts_of(C, node)
  return ref_link(C, node, raw_of(C, p.text), p)
end

INLINE.shortcut_link = function(C, node)
  local p = parts_of(C, node)
  local raw = raw_of(C, p.text)
  return footnote_ref(C, raw) or ref_link(C, node, raw, p)
end

INLINE.image = function(C, node)
  local p = parts_of(C, node)
  local dest, title = p.dest, p.title
  if not dest then
    local def = C.refs[norm_label(p.label or raw_of(C, p.text))]
    if not def then return container(C, node) end
    dest, title = def.dest, def.title
  end
  local alt = p.text and plain(link_content(C, p.text)) or ""
  local t = title and (' title="' .. esc(title) .. '"') or ""
  return '<img src="' .. url_attr(dest) .. '" alt="' .. esc(alt) .. '"' .. t .. ">"
end

inline_node = function(C, node)
  local h = INLINE[node:type()]
  if h then return h(C, node) end
  return container(C, node)
end

local function inline_entry(C, mdnode)
  local s = block_text(C, mdnode)
  local entry = C.cache.next[s]
  if entry then return entry end
  entry = C.cache.map[s]
  if not entry then
    local parser = vim.treesitter.get_string_parser(s, "markdown_inline", { injections = { markdown_inline = "" } })
    local tree = parser:parse()[1]
    entry = { s = s, tree = tree, root = tree:root() }
  end
  C.cache.next[s] = entry
  return entry
end

local function render_inline(C, mdnode, a, b)
  local entry = inline_entry(C, mdnode)
  C.s = entry.s
  local html = children_html(C, entry.root, a or 0, b or #entry.s)
  C.s = nil
  return html
end

local function child_of_type(node, t)
  for _, child in ipairs(node:named_children()) do
    if child:type() == t then return child end
  end
end

local function display_math(C, inl)
  local entry = inline_entry(C, inl)
  local s = entry.s
  local kids = entry.root:named_children()
  local first, last = kids[1], kids[#kids]
  if not first or s:sub(1, sbyte(first)):find("%S") or s:sub(ebyte(last) + 1):find("%S") then return nil end
  if #kids == 1 and first:type() == "latex_block" then
    local a, b = delim_bounds(first, "latex_span_delimiter")
    if a - sbyte(first) < 2 then return nil end
    return s:sub(a + 1, b)
  end
  if #kids >= 2 and first:type() == "backslash_escape" and last:type() == "backslash_escape"
    and s:sub(sbyte(first) + 1, ebyte(first)) == "\\[" and s:sub(sbyte(last) + 1, ebyte(last)) == "\\]" then
    for i = 2, #kids - 1 do
      local k = kids[i]
      if k:type() == "backslash_escape" and s:sub(sbyte(k) + 1, ebyte(k)) == "\\]" then return nil end
    end
    return s:sub(ebyte(first) + 1, sbyte(last))
  end
  return nil
end

local IGNORE = {
  block_continuation = true,
  block_quote_marker = true,
  list_marker_minus = true,
  list_marker_plus = true,
  list_marker_star = true,
  list_marker_dot = true,
  list_marker_parenthesis = true,
  task_list_marker_checked = true,
  task_list_marker_unchecked = true,
}

local BLOCK = {}
local render_block

local function content_children(node)
  local list = {}
  for _, child in ipairs(node:named_children()) do
    if not IGNORE[child:type()] then list[#list + 1] = child end
  end
  return list
end

local function render_children(C, node, out, tight)
  for _, child in ipairs(content_children(node)) do
    render_block(C, child, out, tight)
  end
end

render_block = function(C, node, out, tight, prefix)
  local h = BLOCK[node:type()]
  local ok, html
  if h then
    ok, html = pcall(h, C, node, tight, prefix)
  end
  if not ok then
    C.s, C.in_link, C.in_table = nil, false, false
    html = "<p" .. dl(node) .. ">" .. esc(C.src:sub(sbyte(node) + 1, ebyte(node))) .. "</p>"
  end
  if html and html ~= "" then out[#out + 1] = html end
end

BLOCK.document = function(C, node)
  local out = {}
  render_children(C, node, out)
  return table.concat(out, "\n")
end
BLOCK.section = BLOCK.document

local function heading(C, node, level, inl)
  local content = inl and render_inline(C, inl) or ""
  local id = esc(C.slugger(plain(content):match("^%s*(.-)%s*$")))
  return "<h" .. level .. ' id="' .. id .. '"' .. dl(node) .. '><a class="anchor" href="#' .. id
    .. '" aria-hidden="true">#</a>' .. content .. "</h" .. level .. ">"
end

BLOCK.atx_heading = function(C, node)
  local level = 1
  for _, child in ipairs(node:named_children()) do
    local n = child:type():match("^atx_h(%d)_marker$")
    if n then level = tonumber(n) end
  end
  return heading(C, node, level, child_of_type(node, "inline"))
end

BLOCK.setext_heading = function(C, node)
  local level = child_of_type(node, "setext_h2_underline") and 2 or 1
  local para = child_of_type(node, "paragraph")
  return heading(C, node, level, para and child_of_type(para, "inline"))
end

BLOCK.paragraph = function(C, node, tight, prefix)
  if C.fn_paras[node:id()] then return "" end
  local inl = child_of_type(node, "inline")
  if not inl then return "" end
  local rp = C.ref_paras[node:id()]
  if rp then
    local s = inline_entry(C, inl).s
    if rp.offset >= #s then return "" end
    local html = (prefix or "") .. render_inline(C, inl, rp.offset, #s)
    if tight then return html end
    return '<p data-line="' .. (srow(node) + rp.rows) .. '">' .. html .. "</p>"
  end
  if not prefix then
    local tex = display_math(C, inl)
    if tex then return '<div class="math-display"' .. dl(node) .. ">" .. esc(tex) .. "</div>" end
  end
  local html = (prefix or "") .. render_inline(C, inl)
  if tight then return html end
  return "<p" .. dl(node) .. ">" .. html .. "</p>"
end

local ALERTS = { note = "Note", tip = "Tip", important = "Important", warning = "Warning", caution = "Caution" }

BLOCK.block_quote = function(C, node)
  local kids = content_children(node)
  local first = kids[1]
  local kind, inl
  if first and first:type() == "paragraph" then
    inl = child_of_type(first, "inline")
    if inl then
      local k = inline_entry(C, inl).s:match("^[^\n]*"):match("^%[!(%a+)%]%s*$")
      kind = k and ALERTS[k:lower()] and k:lower()
    end
  end
  local out = {}
  if not kind then
    render_children(C, node, out)
    return "<blockquote" .. dl(node) .. ">\n" .. table.concat(out, "\n") .. "\n</blockquote>"
  end
  out[1] = '<div class="markdown-alert markdown-alert-' .. kind .. '"' .. dl(node) .. '><p class="markdown-alert-title">'
    .. ALERTS[kind] .. "</p>"
  local s = inline_entry(C, inl).s
  local nl = s:find("\n", 1, true)
  if nl then
    local rest = render_inline(C, inl, nl, #s)
    if rest:find("%S") then out[#out + 1] = '<p data-line="' .. (srow(first) + 1) .. '">' .. rest .. "</p>" end
  end
  for i = 2, #kids do render_block(C, kids[i], out) end
  out[#out + 1] = "</div>"
  return table.concat(out, "\n")
end

local function blank_row(C, row)
  local line = C.lines[row + 1]
  return line == nil or line:match("^[%s>]*$") ~= nil
end

local function is_loose(C, items)
  for idx, item in ipairs(items) do
    if idx < #items and last_row(item) > srow(item) and blank_row(C, last_row(item)) then return true end
    local blocks = content_children(item)
    for k = 1, #blocks - 1 do
      for r = last_row(blocks[k]) + 1, srow(blocks[k + 1]) - 1 do
        if blank_row(C, r) then return true end
      end
    end
  end
  return false
end

local CHECKBOX = '<input type="checkbox" class="task-list-item-checkbox" disabled%s> '

BLOCK.list = function(C, node)
  local items = {}
  for _, child in ipairs(node:named_children()) do
    if child:type() == "list_item" then items[#items + 1] = child end
  end
  if #items == 0 then return "" end
  local first_marker = items[1]:named_child(0)
  local mtype = first_marker and first_marker:type() or ""
  local ordered = mtype == "list_marker_dot" or mtype == "list_marker_parenthesis"
  local tight = not is_loose(C, items)
  local has_task = false
  local body = {}
  for _, item in ipairs(items) do
    local checked = child_of_type(item, "task_list_marker_checked")
    local task = checked or child_of_type(item, "task_list_marker_unchecked")
    local box = task and CHECKBOX:format(checked and " checked" or "") or nil
    if task then has_task = true end
    local blocks = content_children(item)
    local out = {}
    for i, blk in ipairs(blocks) do
      if i == 1 and box and blk:type() == "paragraph" then
        render_block(C, blk, out, tight, box)
        box = nil
      else
        render_block(C, blk, out, tight)
      end
    end
    local cls = task and ' class="task-list-item"' or ""
    body[#body + 1] = "<li" .. cls .. dl(item) .. ">" .. (box or "") .. table.concat(out, "\n") .. "</li>"
  end
  local tag = ordered and "ol" or "ul"
  local attrs = has_task and ' class="contains-task-list"' or ""
  if ordered then
    local start = tonumber(C.src:sub(sbyte(first_marker) + 1, ebyte(first_marker)):match("%d+"))
    if start and start ~= 1 then attrs = attrs .. ' start="' .. start .. '"' end
  end
  return "<" .. tag .. attrs .. dl(node) .. ">\n" .. table.concat(body, "\n") .. "\n</" .. tag .. ">"
end

local lang_cache = {}

local function resolve_lang(name)
  name = name:lower()
  local cached = lang_cache[name]
  if cached ~= nil then return cached or nil end
  local candidates = { (name:gsub("%-", "_")), vim.treesitter.language.get_lang(name) }
  local ok_ft, ft = pcall(vim.filetype.match, { filename = "preview." .. name })
  if ok_ft and ft then candidates[#candidates + 1] = vim.treesitter.language.get_lang(ft) end
  local found = false
  for _, cand in ipairs(candidates) do
    if cand and cand:match("^[%w_]+$") then
      local ok, added = pcall(vim.treesitter.language.add, cand)
      if ok and added then
        local okq, query = pcall(vim.treesitter.query.get, cand, "highlights")
        if okq and query then
          found = cand
          break
        end
      end
    end
  end
  lang_cache[name] = found
  return found or nil
end

local class_cache = {}

local function capture_class(name)
  local c = class_cache[name]
  if c then return c end
  local parts, acc = {}, nil
  for seg in name:gmatch("[^%.]+") do
    acc = acc and (acc .. "-" .. seg) or seg
    parts[#parts + 1] = "hl-" .. acc:gsub("[^%w_%-]", "_")
  end
  c = table.concat(parts, " ")
  class_cache[name] = c
  return c
end

local SKIP_CAPTURE = { spell = true, nospell = true, conceal = true }

local function highlight(code, lang)
  local parser = vim.treesitter.get_string_parser(code, lang)
  local tree = parser:parse()[1]
  local query = vim.treesitter.query.get(lang, "highlights")
  local cls = {}
  for id, node in query:iter_captures(tree:root(), code, 0, -1) do
    local name = query.captures[id]
    if name:sub(1, 1) ~= "_" and not SKIP_CAPTURE[name] then
      local c = capture_class(name)
      for i = sbyte(node) + 1, ebyte(node) do cls[i] = c end
    end
  end
  local out, i, n = {}, 1, #code
  while i <= n do
    local c = cls[i]
    local j = i
    while j < n and cls[j + 1] == c do j = j + 1 end
    local s = esc(code:sub(i, j))
    out[#out + 1] = c and ('<span class="' .. c .. '">' .. s .. "</span>") or s
    i = j + 1
  end
  return table.concat(out)
end

BLOCK.fenced_code_block = function(C, node)
  local info = child_of_type(node, "info_string")
  local langnode = info and child_of_type(info, "language")
  local name = langnode and C.src:sub(sbyte(langnode) + 1, ebyte(langnode)) or ""
  local content = child_of_type(node, "code_fence_content")
  local code = content and block_text(C, content) or ""
  if name:lower() == "mermaid" then
    return '<pre class="mermaid"' .. dl(node) .. ">" .. esc(code) .. "</pre>"
  end
  local body
  local lang = name ~= "" and resolve_lang(name)
  if lang then
    local ok, html = pcall(highlight, code, lang)
    body = ok and html or esc(code)
  else
    body = esc(code)
  end
  local cls = name ~= "" and (' class="language-' .. esc(name) .. '"') or ""
  return "<pre" .. dl(node) .. "><code" .. cls .. ">" .. body .. "</code></pre>"
end

BLOCK.indented_code_block = function(C, node)
  local lines = vim.split(block_text(C, node), "\n", { plain = true })
  for i, l in ipairs(lines) do lines[i] = l:gsub("^ ? ? ? ?", "", 1) end
  while #lines > 0 and not lines[#lines]:find("%S") do lines[#lines] = nil end
  return "<pre" .. dl(node) .. "><code>" .. esc(table.concat(lines, "\n")) .. "\n</code></pre>"
end

BLOCK.html_block = function(C, node)
  return passthrough(block_text(C, node))
end

BLOCK.thematic_break = function(_, node)
  return "<hr" .. dl(node) .. ">"
end

BLOCK.link_reference_definition = function() return "" end

local function row_cells(C, row)
  local cells, pos = {}, sbyte(row)
  for _, child in ipairs(row:named_children()) do
    if child:type() == "pipe_table_cell" then
      local _, pipes = C.src:sub(pos + 1, sbyte(child)):gsub("|", "")
      for _ = 1, pipes - 1 do cells[#cells + 1] = false end
      cells[#cells + 1] = child
      pos = ebyte(child)
    end
  end
  return cells
end

local function cell_html(C, cell)
  if not cell then return "" end
  C.in_table = true
  local html = render_inline(C, cell)
  C.in_table = false
  return (html:match("^%s*(.-)%s*$"))
end

BLOCK.pipe_table = function(C, node)
  local header, aligns, rows = nil, {}, {}
  for _, child in ipairs(node:named_children()) do
    local t = child:type()
    if t == "pipe_table_header" then
      header = child
    elseif t == "pipe_table_delimiter_row" then
      for _, cell in ipairs(child:named_children()) do
        if cell:type() == "pipe_table_delimiter_cell" then
          local l = child_of_type(cell, "pipe_table_align_left")
          local r = child_of_type(cell, "pipe_table_align_right")
          aligns[#aligns + 1] = (l and r) and "center" or r and "right" or l and "left" or false
        end
      end
    elseif t == "pipe_table_row" then
      rows[#rows + 1] = child
    end
  end
  if not header then return "" end
  local hcells = row_cells(C, header)
  local ncol = #hcells
  local function attr(i)
    return aligns[i] and (' align="' .. aligns[i] .. '"') or ""
  end
  local out = { "<table" .. dl(node) .. ">", "<thead>", "<tr" .. dl(header) .. ">" }
  for i = 1, ncol do out[#out + 1] = "<th" .. attr(i) .. ">" .. cell_html(C, hcells[i]) .. "</th>" end
  out[#out + 1] = "</tr>"
  out[#out + 1] = "</thead>"
  if #rows > 0 then
    out[#out + 1] = "<tbody>"
    for _, row in ipairs(rows) do
      local cells = row_cells(C, row)
      out[#out + 1] = "<tr" .. dl(row) .. ">"
      for i = 1, ncol do out[#out + 1] = "<td" .. attr(i) .. ">" .. cell_html(C, cells[i]) .. "</td>" end
      out[#out + 1] = "</tr>"
    end
    out[#out + 1] = "</tbody>"
  end
  out[#out + 1] = "</table>"
  return table.concat(out, "\n")
end

local function front_matter(C, node, pattern)
  local lines = vim.split(C.src:sub(sbyte(node) + 1, ebyte(node)), "\n", { plain = true })
  local keys, vals = {}, {}
  for i = 2, #lines - 2 do
    local line = lines[i]
    if line:find("%S") and not line:match("^%s*#") then
      local k, v = line:match(pattern)
      if k then
        keys[#keys + 1] = k
        vals[#keys] = v
      elseif line:match("^%s") and #keys > 0 then
        vals[#keys] = (vals[#keys] .. " " .. line:match("^%s*(.-)%s*$")):match("^%s*(.-)$")
      else
        return ""
      end
    end
  end
  if #keys == 0 then return "" end
  local out = { '<table class="front-matter"' .. dl(node) .. ">", "<thead>", "<tr>" }
  for _, k in ipairs(keys) do out[#out + 1] = "<th>" .. esc(k) .. "</th>" end
  out[#out + 1] = "</tr>"
  out[#out + 1] = "</thead>"
  out[#out + 1] = "<tbody>"
  out[#out + 1] = "<tr>"
  for i = 1, #keys do out[#out + 1] = "<td>" .. esc(vals[i]) .. "</td>" end
  out[#out + 1] = "</tr>"
  out[#out + 1] = "</tbody>"
  out[#out + 1] = "</table>"
  return table.concat(out, "\n")
end

BLOCK.minus_metadata = function(C, node)
  return front_matter(C, node, "^([%w_%-%.]+):%s*(.-)%s*$")
end

BLOCK.plus_metadata = function(C, node)
  return front_matter(C, node, "^([%w_%-%.\"]+)%s*=%s*(.-)%s*$")
end

local CONTAINERS = { document = true, section = true, block_quote = true, list = true, list_item = true }
local FN_DEF = "^%[%^([^%]%s]+)%]:[ \t]*"
local REF_DEF = "^ ? ? ?%[([^%]%^][^%]]*)%]:[ \t]*(%S+)[ \t]*(.-)[ \t]*$"

local function ref_def_line(line)
  local label, dest, rest = line:match(REF_DEF)
  if not label or not label:find("%S") then return nil end
  if dest:sub(1, 1) == "<" then
    if dest:sub(-1) ~= ">" then return nil end
    dest = dest:sub(2, -2)
  end
  local title
  if rest ~= "" then
    title = rest:match('^"(.*)"$') or rest:match("^'(.*)'$") or rest:match("^%((.*)%)$")
    if not title then return nil end
  end
  return label, unescape(dest), title and unescape(title)
end

local function leading_refs(C, para, inl)
  local str = inline_entry(C, inl).s
  local pos, rows = 0, 0
  while pos < #str do
    local nl = str:find("\n", pos + 1, true) or (#str + 1)
    local label, dest, title = ref_def_line(str:sub(pos + 1, nl - 1))
    if not label then break end
    local key = norm_label(label)
    if not C.refs[key] then C.refs[key] = { dest = dest, title = title } end
    pos, rows = nl, rows + 1
  end
  if rows > 0 then C.ref_paras[para:id()] = { offset = pos, rows = rows } end
end

local function collect(C, node)
  for _, child in ipairs(node:named_children()) do
    local t = child:type()
    if CONTAINERS[t] then
      collect(C, child)
    elseif t == "link_reference_definition" then
      local label = child_of_type(child, "link_label")
      local dest = child_of_type(child, "link_destination")
      if label then
        local raw = C.src:sub(sbyte(label) + 2, ebyte(label) - 1)
        if raw:sub(1, 1) == "^" then
          local key = norm_label(raw:sub(2))
          if not C.fn_defs[key] then
            local from = dest and sbyte(dest) or ebyte(label) + 1
            C.fn_defs[key] = { raw = C.src:sub(from + 1, ebyte(child)):match("^%s*(.-)%s*$"), row = srow(child) }
          end
        else
          local key = norm_label(raw)
          if not C.refs[key] and dest then
            local d = C.src:sub(sbyte(dest) + 1, ebyte(dest))
            if d:sub(1, 1) == "<" and d:sub(-1) == ">" then d = d:sub(2, -2) end
            local title = child_of_type(child, "link_title")
            C.refs[key] = {
              dest = unescape(d),
              title = title and unescape(C.src:sub(sbyte(title) + 2, ebyte(title) - 1)) or nil,
            }
          end
        end
      end
    elseif t == "paragraph" then
      local inl = child_of_type(child, "inline")
      if inl and C.src:match("^%[[^%]%^][^%]]*%]:", sbyte(inl) + 1) then pcall(leading_refs, C, child, inl) end
      if inl and C.src:match(FN_DEF, sbyte(inl) + 1) then
        C.fn_paras[child:id()] = true
        local str = inline_entry(C, inl).s
        local starts = {}
        local p = 0
        while p do
          local _, e, id = str:find(FN_DEF, p + 1)
          if id then starts[#starts + 1] = { id = id, at = p, body = e } end
          p = str:find("\n", p + 1, true)
        end
        for i, st in ipairs(starts) do
          local key = norm_label(st.id)
          if not C.fn_defs[key] then
            local stop = starts[i + 1] and (starts[i + 1].at - 1) or #str
            local row = srow(inl) + select(2, str:sub(1, st.at):gsub("\n", ""))
            C.fn_defs[key] = { inline = inl, a = st.body, b = stop, row = row }
          end
        end
      end
    end
  end
end

local function footnotes(C)
  if #C.fn_order == 0 then return "" end
  local out = { '<section class="footnotes" data-footnotes>', "<ol>" }
  local i = 1
  while i <= #C.fn_order do
    local key = C.fn_order[i]
    local def = C.fn_defs[key]
    local body
    if def.inline then
      body = render_inline(C, def.inline, def.a, def.b)
    else
      body = text(C, def.raw)
    end
    local backs = {}
    for k = 1, C.fn_refs[key] or 1 do
      local ref = "fnref-" .. i .. (k > 1 and ("-" .. k) or "")
      backs[#backs + 1] = '<a href="#' .. ref .. '" class="data-footnote-backref" aria-label="Back to reference ' .. i
        .. '">&#8617;</a>'
    end
    out[#out + 1] = '<li id="fn-' .. i .. '" data-line="' .. def.row .. '"><p>' .. body .. " " .. table.concat(backs, " ")
      .. "</p></li>"
    i = i + 1
  end
  out[#out + 1] = "</ol>"
  out[#out + 1] = "</section>"
  return table.concat(out, "\n")
end

local function parse_string(src)
  local ok, parser = pcall(vim.treesitter.get_string_parser, src, "markdown", { injections = { markdown = "" } })
  if not ok then return nil end
  local okp, trees = pcall(parser.parse, parser)
  return okp and trees and trees[1] or nil
end

function M.render_lines(lines, opts)
  opts = opts or {}
  local src = table.concat(lines, "\n") .. "\n"
  local tree = opts.tree or parse_string(src)
  if not tree then return "<pre>" .. esc(src) .. "</pre>\n" end
  local cache = opts.cache or {}
  cache.map, cache.next = cache.map or {}, {}
  local C = {
    src = src,
    lines = lines,
    cache = cache,
    refs = {},
    fn_defs = {},
    fn_paras = {},
    ref_paras = {},
    fn_order = {},
    fn_num = {},
    fn_refs = {},
    slugger = Slug.new(),
  }
  collect(C, tree:root())
  local out = {}
  render_block(C, tree:root(), out)
  local notes = footnotes(C)
  if notes ~= "" then out[#out + 1] = notes end
  cache.map, cache.next = cache.next, nil
  return table.concat(out, "\n") .. "\n"
end

local function buffer_tree(buf)
  if vim.treesitter.language.get_lang(vim.bo[buf].filetype) ~= "markdown" then return nil end
  local ok, parser = pcall(vim.treesitter.get_parser, buf, "markdown", { error = false })
  if not ok or not parser then return nil end
  local okp, trees = pcall(parser.parse, parser)
  return okp and trees and trees[1] or nil
end

function M.render(buf, cache)
  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  return M.render_lines(lines, { tree = buffer_tree(buf), cache = cache })
end

return M
