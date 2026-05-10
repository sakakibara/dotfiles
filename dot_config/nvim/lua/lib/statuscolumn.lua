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

local _vd_cache = {}
local function visible_distance(bufnr, lnum, cur)
  local tick = vim.api.nvim_buf_get_changedtick(bufnr)
  local entry = _vd_cache[bufnr]
  if not entry or entry.tick ~= tick or entry.cur ~= cur then
    entry = { tick = tick, cur = cur, dist = {} }
    _vd_cache[bufnr] = entry
  end
  if entry.dist[lnum] then return entry.dist[lnum] end
  local lo, hi = math.min(lnum, cur), math.max(lnum, cur)
  local visible = 0
  local i = lo + 1
  while i <= hi do
    local fold_start = vim.fn.foldclosed(i)
    if fold_start == -1 then
      visible = visible + 1
      i = i + 1
    elseif fold_start == i then
      visible = visible + 1
      i = vim.fn.foldclosedend(i) + 1
    else
      i = vim.fn.foldclosedend(i) + 1
    end
  end
  entry.dist[lnum] = visible
  return visible
end

local function number_for(lnum, relnum, virtnum)
  if virtnum and virtnum ~= 0 then return "    " end
  local nu = vim.wo.number
  local rnu = vim.wo.relativenumber
  if not nu and not rnu then return "    " end
  local n
  if rnu and relnum and relnum ~= 0 then
    n = visible_distance(vim.api.nvim_get_current_buf(), lnum, vim.fn.line("."))
  elseif rnu and not nu then
    n = 0
  else
    n = lnum
  end
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
