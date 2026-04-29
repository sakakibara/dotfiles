-- lua/lib/statuscolumn.lua
local M = {}

local function gitsign_for(buf, lnum)
  local gs = vim.fn.sign_getplaced(buf, { group = "gitsigns_vimfn_signs_", lnum = lnum })[1]
  if not gs or not gs.signs or #gs.signs == 0 then return " " end
  local sign = gs.signs[1]
  local def = vim.fn.sign_getdefined(sign.name)[1]
  if def and def.text then
    return "%#" .. (def.texthl or "GitSignsAdd") .. "#" .. def.text .. "%*"
  end
  return " "
end

local function number_for(lnum, relnum, virtnum)
  if virtnum and virtnum ~= 0 then return "    " end
  local n
  if vim.wo.relativenumber and relnum and relnum ~= 0 then n = relnum
  else n = lnum end
  return string.format("%4d", n)
end

local function fold_for(lnum)
  local fillchars = vim.opt.fillchars:get()
  if vim.fn.foldlevel(lnum) == 0 then return " " end
  if vim.fn.foldclosed(lnum) > 0 then
    return "%#FoldColumn#" .. (fillchars.foldclose or ">") .. "%*"
  elseif vim.fn.foldlevel(lnum) > vim.fn.foldlevel(lnum - 1) then
    return "%#FoldColumn#" .. (fillchars.foldopen or "v") .. "%*"
  end
  return " "
end

function M.render()
  local lnum = vim.v.lnum
  local relnum = vim.v.relnum
  local virtnum = vim.v.virtnum
  local buf = vim.api.nvim_get_current_buf()

  return table.concat({
    "%s",  -- signs column (neovim handles this)
    number_for(lnum, relnum, virtnum),
    " ",
    gitsign_for(buf, lnum),
    fold_for(lnum),
    " ",
  })
end


return M
