local T = require("tests.helpers")

local function fold_segments(lines, ft, lang)
  vim.cmd("tabnew")
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].filetype = ft
  if lang then
    vim.treesitter.start(buf, lang)
    vim.treesitter.get_parser(buf):parse(true)
  end
  vim.wo.foldmethod = "manual"
  vim.wo.foldlevel = 0
  vim.cmd("1," .. #lines .. "fold")
  local got
  _G.__fold_spec_capture = function()
    got = require("lib.fold").foldtext()
    return ""
  end
  vim.wo.foldtext = "v:lua.__fold_spec_capture()"
  vim.fn.foldtextresult(1)
  _G.__fold_spec_capture = nil
  return got, buf
end

local function per_byte_reference(buf, line)
  local out = {}
  for col = 1, #line do
    local caps = vim.treesitter.get_captures_at_pos(buf, 0, col - 1)
    local hl = "Folded"
    for i = #caps, 1, -1 do
      local name = caps[i].capture
      if name and not name:find("^_") then hl = "@" .. name; break end
    end
    out[col] = hl
  end
  return out
end

T.describe("lib.fold.foldtext", function()
  T.it("paints the fold line with the same captures as a per-byte lookup, without per-byte queries", function()
    local line = "local function greet(name) return 'hi ' .. name end -- {{{1"
    local real = vim.treesitter.get_captures_at_pos
    local calls = 0
    vim.treesitter.get_captures_at_pos = function(...) calls = calls + 1; return real(...) end
    local ok, err = pcall(function()
      local segs, buf = fold_segments({ line, "", "x = 1" }, "lua", "lua")
      T.eq(calls, 0)
      local ref = per_byte_reference(buf, line)
      local stripped = line:gsub("%s*{{{%d*%s*$", "")
      local want = {}
      for col = 1, #stripped do
        local hl = ref[col]
        local last = want[#want]
        if last and last[2] == hl then last[1] = last[1] .. line:sub(col, col)
        else want[#want + 1] = { line:sub(col, col), hl } end
      end
      for i = 1, #want do T.eq(segs[i], want[i], "segment " .. i) end
      T.truthy(#want > 3, "several highlight groups")
    end)
    vim.treesitter.get_captures_at_pos = real
    vim.cmd("tabclose!")
    if not ok then error(err, 0) end
  end)

  T.it("uses the plain branch when the filetype has no parser", function()
    local segs = fold_segments({ "  plain text {{{", "b" }, "zzz-no-parser", nil)
    T.eq(segs[1], { "plain text", "Folded" })
    vim.cmd("tabclose!")
  end)
end)
