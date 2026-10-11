local M = {}

local glyph_cache = {}

local function glyphs(win)
  local fc = vim.wo[win].fillchars
  local g = glyph_cache[fc]
  if not g then
    local open, close = "v", ">"
    for item in vim.gsplit(fc, ",", { plain = true }) do
      local k, v = item:match("^(%w+):(.+)$")
      if k == "foldopen" then open = v elseif k == "foldclose" then close = v end
    end
    g = { open = open, close = close }
    glyph_cache[fc] = g
  end
  return g
end

local function number_for(win, lnum, relnum, virtnum)
  if virtnum and virtnum ~= 0 then return "    " end
  local nu = vim.wo[win].number
  local rnu = vim.wo[win].relativenumber
  if not nu and not rnu then return "    " end
  local n
  if rnu and relnum and relnum ~= 0 then
    n = relnum
  elseif rnu and not nu then
    n = 0
  else
    n = lnum
  end
  return string.format("%4d", n)
end

local function fold_for(win, lnum)
  local level = vim.fn.foldlevel(lnum)
  if level == 0 then return " " end
  if vim.fn.foldclosed(lnum) > 0 then
    return "%#FoldColumn#" .. glyphs(win).close .. "%*"
  elseif level > vim.fn.foldlevel(lnum - 1) then
    return "%#FoldColumn#" .. glyphs(win).open .. "%*"
  end
  return " "
end

function M.render()
  local win = vim.g.statusline_winid or vim.api.nvim_get_current_win()
  local lnum = vim.v.lnum

  return table.concat({
    "%s",  -- signs column (neovim handles this)
    number_for(win, lnum, vim.v.relnum, vim.v.virtnum),
    " ",
    vim.api.nvim_win_call(win, function() return fold_for(win, lnum) end),
    " ",
  })
end

return M
