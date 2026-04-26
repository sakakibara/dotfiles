local T = require("tests.helpers")
local D = require("lib.colors.detect")

local function make_buf(lines)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  return buf
end

T.describe("lib.colors.detect regex fallback", function()
  T.it("finds hex literals on multiple lines", function()
    D._regex_filetypes = { [""] = true }
    local buf = make_buf({ "color: #ff0000;", "bg: #00ff00;" })
    local results = D.detect(buf, 0, 1)
    D._regex_filetypes = {}
    T.eq(#results, 2)
    T.eq(results[1].lnum, 0)
    T.eq(results[2].lnum, 1)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("respects 1000-char per-line cap", function()
    D._regex_filetypes = { [""] = true }
    local big = "a:" .. string.rep("x", 1500) .. " #ff0000;"
    local buf = make_buf({ big })
    local results = D.detect(buf, 0, 0)
    D._regex_filetypes = {}
    T.eq(#results, 0, "should skip lines > 1000 chars")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("respects 256KB total visible cap", function()
    -- 300 lines × 1000 chars = 300KB > 256KB
    local lines = {}
    for i = 1, 300 do lines[i] = string.rep("x", 999) .. " #ff0000" end
    local buf = make_buf(lines)
    local results = D.detect(buf, 0, 299)
    T.eq(#results, 0, "should skip when viewport bytes > 256KB")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

T.describe("lib.colors.detect TS dispatch", function()
  T.it("uses TS for CSS buffers", function()
    local buf = make_buf({
      ".btn {",
      "  color: #ff0000;",
      "  /* #00ff00 — should be skipped (inside a comment) */",
      "}",
    })
    vim.bo[buf].filetype = "css"
    local results = D.detect(buf, 0, 3)
    -- TS query should exclude the comment
    T.eq(#results, 1, "expected 1 (excluding comment), got " .. #results)
    T.eq(results[1].lnum, 1)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

T.describe("lib.colors.detect JSX class attributes", function()
  T.it("detects bg-red-500 inside className", function()
    local buf = make_buf({
      [[<div className="bg-red-500 p-4">hi</div>]],
    })
    vim.bo[buf].filetype = "javascriptreact"
    local results = D.detect(buf, 0, 0)
    T.eq(#results, 1)
    T.eq(results[1].color.source.tailwind_class, "red-500")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)

T.describe("lib.colors.detect regex gate", function()
  T.it("does NOT run regex on a TS-less filetype by default", function()
    D._regex_filetypes = {}  -- explicit reset for test isolation
    local buf = make_buf({ "#define MAX_DEPTH 64", "color: #ff0000;" })
    vim.bo[buf].filetype = "c"
    local results = D.detect(buf, 0, 1)
    T.eq(#results, 0, "expected 0 detections in C without TS or whitelist; got " .. #results)
    vim.api.nvim_buf_delete(buf, { force = true })
  end)

  T.it("runs regex on a whitelisted ft", function()
    D._regex_filetypes = { ["c"] = true }
    local buf = make_buf({ "color: #ff0000;" })
    vim.bo[buf].filetype = "c"
    local results = D.detect(buf, 0, 0)
    T.truthy(#results >= 1, "expected at least 1 detection on whitelisted ft")
    vim.api.nvim_buf_delete(buf, { force = true })
    D._regex_filetypes = {}
  end)

  T.it("CSS/JSX/HTML still work because they have TS queries", function()
    D._regex_filetypes = {}
    local buf = make_buf({ "color: #ff0000;" })
    vim.bo[buf].filetype = "css"
    local results = D.detect(buf, 0, 0)
    T.truthy(#results >= 1, "CSS should still detect via TS")
    vim.api.nvim_buf_delete(buf, { force = true })
  end)
end)
