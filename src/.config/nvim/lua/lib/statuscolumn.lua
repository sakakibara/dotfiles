local M = {}

local fold_glyphs

vim.api.nvim_create_autocmd("OptionSet", {
  group = vim.api.nvim_create_augroup("Lib.statuscolumn", { clear = true }),
  pattern = "fillchars",
  callback = function() fold_glyphs = nil end,
})

local function glyphs()
  if not fold_glyphs then
    local fillchars = vim.opt.fillchars:get()
    fold_glyphs = { open = fillchars.foldopen or "v", close = fillchars.foldclose or ">" }
  end
  return fold_glyphs
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

local function fold_for(lnum)
  local level = vim.fn.foldlevel(lnum)
  if level == 0 then return " " end
  if vim.fn.foldclosed(lnum) > 0 then
    return "%#FoldColumn#" .. glyphs().close .. "%*"
  elseif level > vim.fn.foldlevel(lnum - 1) then
    return "%#FoldColumn#" .. glyphs().open .. "%*"
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
    vim.api.nvim_win_call(win, function() return fold_for(lnum) end),
    " ",
  })
end

return M
