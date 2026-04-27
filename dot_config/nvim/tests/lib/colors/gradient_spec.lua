local T = require("tests.helpers")

local function make_buf(lines, ft)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  if ft then vim.bo[buf].filetype = ft end
  return buf
end

T.describe("lib.colors.gradient", function()
  T.it("renders an inline strip for a 2-stop linear-gradient", function()
    local G = require("lib.colors.gradient")
    local buf = make_buf({
      "background: linear-gradient(90deg, #ff0000, #00ff00);",
    }, "css")
    G.render(buf, 0, 0)
    local marks = vim.api.nvim_buf_get_extmarks(buf, G.ns, 0, -1, { details = true })
    T.eq(#marks, 1, "expected 1 gradient mark")
    -- 1 leading space + 2 stop cells = 3 virt_text chunks
    local detail = marks[1][4]
    T.truthy(detail.virt_text, "no virt_text")
    T.eq(#detail.virt_text, 3)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("renders 3 stops for a 3-stop gradient", function()
    local G = require("lib.colors.gradient")
    local buf = make_buf({
      "bg: linear-gradient(to right, #ff0000, #00ff00, #0000ff);",
    }, "css")
    G.render(buf, 0, 0)
    local marks = vim.api.nvim_buf_get_extmarks(buf, G.ns, 0, -1, { details = true })
    T.eq(#marks, 1)
    local detail = marks[1][4]
    -- 1 leading space + 3 stops = 4 chunks
    T.eq(#detail.virt_text, 4)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("handles radial-gradient and conic-gradient", function()
    local G = require("lib.colors.gradient")
    local buf = make_buf({
      "a { bg: radial-gradient(#000, #fff); }",
      "b { bg: conic-gradient(#ff0000, #0000ff); }",
    }, "css")
    G.render(buf, 0, 1)
    local marks = vim.api.nvim_buf_get_extmarks(buf, G.ns, 0, -1, {})
    T.eq(#marks, 2, "expected 1 mark per line")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("ignores gradient calls with fewer than 2 colors", function()
    local G = require("lib.colors.gradient")
    -- Direction only — no colors. parse_all returns 0 stops.
    local buf = make_buf({ "bg: linear-gradient(90deg);" }, "css")
    G.render(buf, 0, 0)
    local marks = vim.api.nvim_buf_get_extmarks(buf, G.ns, 0, -1, {})
    T.eq(#marks, 0)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("toggle flips per-buffer state and renders/clears", function()
    local G = require("lib.colors.gradient")
    local buf = make_buf({
      "bg: linear-gradient(90deg, #ff0000, #00ff00);",
    }, "css")
    vim.api.nvim_set_current_buf(buf)

    T.eq(G.toggle(buf), true)
    T.eq(G.is_enabled(buf), true)
    local marks = vim.api.nvim_buf_get_extmarks(buf, G.ns, 0, -1, {})
    T.eq(#marks, 1)

    T.eq(G.toggle(buf), false)
    marks = vim.api.nvim_buf_get_extmarks(buf, G.ns, 0, -1, {})
    T.eq(#marks, 0)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

T.describe("lib.colors :ColorGradient command", function()
  T.it("registers :ColorGradient after setup()", function()
    package.loaded["lib.colors"] = nil
    require("lib").init()
    Lib.colors.setup({})
    local cmds = vim.api.nvim_get_commands({})
    T.truthy(cmds.ColorGradient, ":ColorGradient not registered")
  end)
end)
