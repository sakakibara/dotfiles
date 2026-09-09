local T = require("tests.helpers")

local MD = require("lib.markdown")

local function buf_with(lines)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  return buf
end

local function lines_of(buf)
  return vim.api.nvim_buf_get_lines(buf, 0, -1, false)
end

T.describe("lib.markdown.toggle_line", function()
  T.it("checks an unchecked box", function()
    local buf = buf_with({ "- [ ] write it" })
    T.eq(MD.toggle_line(buf, 0), 1)
    T.eq(lines_of(buf), { "- [x] write it" })
  end)

  T.it("unchecks a checked box, upper or lower case", function()
    local buf = buf_with({ "- [x] one", "- [X] two" })
    MD.toggle_line(buf, 0)
    MD.toggle_line(buf, 1)
    T.eq(lines_of(buf), { "- [ ] one", "- [ ] two" })
  end)

  T.it("handles every list marker CommonMark allows", function()
    local buf = buf_with({ "* [ ] star", "+ [ ] plus", "1. [ ] ordered", "2) [ ] paren" })
    for row = 0, 3 do MD.toggle_line(buf, row) end
    T.eq(lines_of(buf), { "* [x] star", "+ [x] plus", "1. [x] ordered", "2) [x] paren" })
  end)

  T.it("sees through nested block quotes", function()
    local buf = buf_with({ "> - [ ] quoted", "> > - [ ] deeper" })
    for row = 0, 1 do MD.toggle_line(buf, row) end
    T.eq(lines_of(buf), { "> - [x] quoted", "> > - [x] deeper" })
  end)

  T.it("toggles a nested list item", function()
    local buf = buf_with({ "- parent", "  - [ ] nested" })
    T.eq(MD.toggle_line(buf, 1), 1)
    T.eq(lines_of(buf), { "- parent", "  - [x] nested" })
  end)

  T.it("leaves an indented code block alone, marker or not", function()
    local buf = buf_with({ "    - [ ] indented" })
    T.eq(MD.toggle_line(buf, 0), 0)
    T.eq(lines_of(buf), { "    - [ ] indented" })
  end)

  T.it("leaves a line that carries no task marker alone", function()
    local buf = buf_with({ "- plain item", "not a list", "" })
    for row = 0, 2 do T.eq(MD.toggle_line(buf, row), 0) end
    T.eq(lines_of(buf), { "- plain item", "not a list", "" })
  end)

  T.it("never inserts a marker into a bare list item", function()
    local buf = buf_with({ "- [] malformed", "-[ ] no space" })
    for row = 0, 1 do T.eq(MD.toggle_line(buf, row), 0) end
    T.eq(lines_of(buf), { "- [] malformed", "-[ ] no space" })
  end)
end)

T.describe("lib.markdown.toggle_range", function()
  T.it("toggles every marked line in the range and counts them", function()
    local buf = buf_with({ "- [ ] a", "plain", "- [x] b", "- [ ] c" })
    T.eq(MD.toggle_range(buf, 0, 3), 3)
    T.eq(lines_of(buf), { "- [x] a", "plain", "- [ ] b", "- [x] c" })
  end)

  T.it("accepts the range ends in either order", function()
    local buf = buf_with({ "- [ ] a", "- [ ] b" })
    T.eq(MD.toggle_range(buf, 1, 0), 2)
    T.eq(lines_of(buf), { "- [x] a", "- [x] b" })
  end)

  T.it("counts nothing when the range holds no markers", function()
    local buf = buf_with({ "one", "two" })
    T.eq(MD.toggle_range(buf, 0, 1), 0)
  end)
end)

T.describe("lib.markdown regex fallback", function()
  local real_get_parser = vim.treesitter.get_parser
  local function without_parser(fn)
    vim.treesitter.get_parser = function() error("no parser") end
    local ok, err = pcall(fn)
    vim.treesitter.get_parser = real_get_parser
    if not ok then error(err) end
  end

  T.it("checks and unchecks a box", function()
    without_parser(function()
      local buf = buf_with({ "- [ ] write it", "* [x] done" })
      T.eq(MD.toggle_line(buf, 0), 1)
      T.eq(MD.toggle_line(buf, 1), 1)
      T.eq(lines_of(buf), { "- [x] write it", "* [ ] done" })
    end)
  end)

  T.it("reaches a box behind a blockquote and a numbered marker", function()
    without_parser(function()
      local buf = buf_with({ "> 1. [ ] quoted", "> > - [X] deeper" })
      MD.toggle_line(buf, 0)
      MD.toggle_line(buf, 1)
      T.eq(lines_of(buf), { "> 1. [x] quoted", "> > - [ ] deeper" })
    end)
  end)

  T.it("toggles a marker at the end of the line, with and without the parser", function()
    without_parser(function()
      local buf = buf_with({ "- [ ]" })
      T.eq(MD.toggle_line(buf, 0), 1)
      T.eq(lines_of(buf), { "- [x]" })
    end)
    local buf = buf_with({ "- [ ]", "  - [ ]" })
    T.eq(MD.toggle_line(buf, 0), 1)
    T.eq(MD.toggle_line(buf, 1), 1)
    T.eq(lines_of(buf), { "- [x]", "  - [x]" })
    local code = buf_with({ "```", "- [ ]", "```", "para", "", "    - [ ]" })
    T.eq(MD.toggle_line(code, 1), 0)
    T.eq(MD.toggle_line(code, 5), 0)
    T.eq(lines_of(code), { "```", "- [ ]", "```", "para", "", "    - [ ]" })
  end)


  T.it("ignores a line with no marker", function()
    without_parser(function()
      local buf = buf_with({ "- plain item", "[ ] no list marker" })
      T.eq(MD.toggle_line(buf, 0), 0)
      T.eq(MD.toggle_line(buf, 1), 0)
    end)
  end)
end)
