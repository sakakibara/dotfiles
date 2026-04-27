local T = require("tests.helpers")

local function make_buf(lines, ft)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  if ft then vim.bo[buf].filetype = ft end
  return buf
end

T.describe("lib.colors.contrast", function()
  T.it("renders a virt-text mark on a line with two distinct colors", function()
    local CT = require("lib.colors.contrast")
    local buf = make_buf({ "color: #ffffff; background: #000000;" }, "css")
    CT.render(buf, 0, 0)
    local marks = vim.api.nvim_buf_get_extmarks(buf, CT.ns, 0, -1, { details = true })
    T.eq(#marks, 1, "expected 1 contrast mark")
    local detail = marks[1][4]
    T.truthy(detail.virt_text, "no virt_text on the mark")
    -- 21:1 + AAA in the rendered text
    T.truthy(detail.virt_text[1][1]:match("21%.0:1 AAA"),
             "expected '21.0:1 AAA', got " .. detail.virt_text[1][1])
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("skips lines that have only one color", function()
    local CT = require("lib.colors.contrast")
    local buf = make_buf({ "color: #ff0000;" }, "css")
    CT.render(buf, 0, 0)
    local marks = vim.api.nvim_buf_get_extmarks(buf, CT.ns, 0, -1, {})
    T.eq(#marks, 0)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("skips lines where both colors are identical", function()
    local CT = require("lib.colors.contrast")
    local buf = make_buf({ "color: #ff0000; bg: #ff0000;" }, "css")
    CT.render(buf, 0, 0)
    local marks = vim.api.nvim_buf_get_extmarks(buf, CT.ns, 0, -1, {})
    T.eq(#marks, 0)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("toggle flips per-buffer state and renders/clears", function()
    local CT = require("lib.colors.contrast")
    local buf = make_buf({ "color: #ffffff; background: #000000;" }, "css")
    vim.api.nvim_set_current_buf(buf)

    -- Off → on
    local first = CT.toggle(buf)
    T.eq(first, true)
    T.eq(CT.is_enabled(buf), true)
    local marks = vim.api.nvim_buf_get_extmarks(buf, CT.ns, 0, -1, {})
    T.eq(#marks, 1)

    -- On → off (clears marks)
    local second = CT.toggle(buf)
    T.eq(second, false)
    marks = vim.api.nvim_buf_get_extmarks(buf, CT.ns, 0, -1, {})
    T.eq(#marks, 0)

    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("uses pre-computed hits when supplied (no double detect)", function()
    local CT = require("lib.colors.contrast")
    local C  = require("lib.colors.color")
    local buf = make_buf({ "two colors: #ffffff #000000" }, "css")
    -- Synthesize hits as if D.detect produced them.
    local hits = {
      { lnum = 0, color = C.from_hex("#ffffff") },
      { lnum = 0, color = C.from_hex("#000000") },
    }
    CT.render(buf, 0, 0, hits)
    local marks = vim.api.nvim_buf_get_extmarks(buf, CT.ns, 0, -1, {})
    T.eq(#marks, 1)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

T.describe("lib.colors :ColorContrast command", function()
  T.it("registers :ColorContrast after setup()", function()
    package.loaded["lib.colors"] = nil
    require("lib").init()
    Lib.colors.setup({})
    local cmds = vim.api.nvim_get_commands({})
    T.truthy(cmds.ColorContrast, ":ColorContrast not registered")
  end)
end)
