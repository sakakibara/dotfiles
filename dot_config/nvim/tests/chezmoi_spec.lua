local T = require("tests.helpers")

local function fresh_module()
  package.loaded["lib.chezmoi"] = nil
  return require("lib.chezmoi")
end

local function with_buf(lines, fn)
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  local ok, err = pcall(fn, buf)
  vim.api.nvim_buf_delete(buf, { force = true })
  if not ok then error(err) end
end

T.describe("lib.chezmoi._template_regions", function()
  local chezmoi = fresh_module()

  T.it("finds a single inline directive", function()
    with_buf({ 'export LANG={{ .locale }}' }, function(buf)
      local regs = chezmoi._template_regions(buf)
      T.eq(#regs, 1)
      local r = regs[1][1]  -- first node range of the first region
      T.eq(r[1], 0)         -- start row
      T.eq(r[2], 12)        -- start col (col of first '{')
      T.eq(r[3], 12)        -- start byte
      T.eq(r[4], 0)         -- end row
      T.eq(r[5], 25)        -- end col (one past last '}')
      T.eq(r[6], 25)        -- end byte
    end)
  end)

  T.it("finds multiple directives on one line", function()
    with_buf({ 'a {{x}} b {{y}} c' }, function(buf)
      local regs = chezmoi._template_regions(buf)
      T.eq(#regs, 2)
    end)
  end)

  T.it("finds directives across multiple lines", function()
    with_buf({
      '{{ if .foo }}',
      '  bar',
      '{{ end }}',
    }, function(buf)
      local regs = chezmoi._template_regions(buf)
      T.eq(#regs, 2)
      T.eq(regs[1][1][1], 0)  -- first directive on row 0
      T.eq(regs[2][1][1], 2)  -- second directive on row 2
    end)
  end)

  T.it("returns empty when no directives", function()
    with_buf({ 'export LANG=en_US.UTF-8', 'no templates here' }, function(buf)
      T.eq(chezmoi._template_regions(buf), {})
    end)
  end)

  T.it("computes byte offsets across multiple lines", function()
    -- "abc\nde{{x}}f" — line 0 has 3 bytes + 1 newline = 4-byte prefix
    with_buf({ 'abc', 'de{{x}}f' }, function(buf)
      local regs = chezmoi._template_regions(buf)
      T.eq(#regs, 1)
      local r = regs[1][1]
      T.eq(r[1], 1)  -- row 1
      T.eq(r[2], 2)  -- col 2 (start of '{{')
      T.eq(r[3], 4 + 2)  -- 4 bytes of "abc\n" prefix + 2 cols on line 1
      T.eq(r[4], 1)
      T.eq(r[5], 7)  -- col 7 (one past last '}')
      T.eq(r[6], 4 + 7)
    end)
  end)
end)
