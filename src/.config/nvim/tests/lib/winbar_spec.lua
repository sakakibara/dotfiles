local T = require("tests.helpers")

local opened

local function fresh()
  opened = nil
  package.loaded["lib.winbar_menu"] = {
    open = function(items, opts) opened = { items = items, opts = opts } end,
    close_all = function() end,
  }
  package.loaded["lib.winbar"] = nil
  local W = require("lib.winbar")
  package.loaded["lib.winbar_menu"] = nil
  return W
end

local function segments(bar)
  local out = {}
  for id, content in bar:gmatch("%%(%d+)@v:lua%.Lib%.winbar%.click@(.-)%%X") do
    out[#out + 1] = { id = tonumber(id), label = vim.trim((content:gsub("%%#[^#]*#", ""))) }
  end
  return out
end

local function render_for(name)
  local W = fresh()
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(buf, name)
  local prev = vim.api.nvim_get_current_buf()
  vim.api.nvim_set_current_buf(buf)
  local bar = W.render()
  vim.api.nvim_set_current_buf(prev)
  vim.api.nvim_buf_delete(buf, { force = true })
  return W, bar
end

local function click(W, segs, label)
  for _, s in ipairs(segs) do
    if s.label == label then
      W.click(s.id, 1, "l", "")
      return opened
    end
  end
  error("no segment labelled " .. label)
end

T.describe("lib.winbar path segments", function()
  T.it("renders a root segment for an absolute path outside home", function()
    local W, bar = render_for("/usr/bin/env")
    local segs = segments(bar)
    T.eq(segs[1].label, "/")
    T.eq(segs[2].label, "usr")
    T.eq(segs[3].label, "bin")
    for _, s in ipairs(segs) do T.truthy(s.label ~= "", "no empty segment") end
    local menu = click(W, segs, "usr")
    T.truthy(menu and #menu.items > 0, "usr listing opened")
    for _, item in ipairs(menu.items) do
      T.truthy(item.file:sub(1, 5) == "/usr/", item.file)
    end
    menu = click(W, segs, "/")
    for _, item in ipairs(menu.items) do
      T.truthy(item.file:match("^/[^/]"), item.file)
    end
  end)

  T.it("lists absolute paths for directories under the cwd", function()
    local cwd = vim.uv.cwd()
    local W, bar = render_for(cwd .. "/lua/lib/path.lua")
    local segs = segments(bar)
    T.eq(segs[1].label, "lua")
    local menu = click(W, segs, "lua")
    T.truthy(menu and #menu.items > 0, "lua listing opened")
    for _, item in ipairs(menu.items) do
      T.truthy(item.file:sub(1, #cwd + 5) == cwd .. "/lua/", item.file)
    end
  end)

  T.it("keeps a window's click ids stable across renders", function()
    local W = fresh()
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, "/usr/bin/env")
    local prev = vim.api.nvim_get_current_buf()
    vim.api.nvim_set_current_buf(buf)
    local first = segments(W.render())
    for _ = 1, 600 do W.render() end
    local again = segments(W.render())
    vim.api.nvim_set_current_buf(prev)
    vim.api.nvim_buf_delete(buf, { force = true })
    T.eq(vim.tbl_map(function(s) return s.id end, again), vim.tbl_map(function(s) return s.id end, first))
    T.truthy(click(W, first, "usr"), "id from the first render still dispatches")
  end)
end)
