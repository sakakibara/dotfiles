---@class util.ui
local M = {}

function M.blend(color1, color2, alpha)
  color1 = type(color1) == "number" and string.format("#%06x", color1) or color1
  color2 = type(color2) == "number" and string.format("#%06x", color2) or color2
  local r1, g1, b1 = color1:match("#(%x%x)(%x%x)(%x%x)")
  local r2, g2, b2 = color2:match("#(%x%x)(%x%x)(%x%x)")
  local r = tonumber(r1, 16) * alpha + tonumber(r2, 16) * (1 - alpha)
  local g = tonumber(g1, 16) * alpha + tonumber(g2, 16) * (1 - alpha)
  local b = tonumber(b1, 16) * alpha + tonumber(b2, 16) * (1 - alpha)
  return "#"
    .. string.format("%02x", math.min(255, math.max(r, 0)))
    .. string.format("%02x", math.min(255, math.max(g, 0)))
    .. string.format("%02x", math.min(255, math.max(b, 0)))
end

function M.dim(color, n)
  return M.blend(color, "#000000", n)
end

function M.get_signs(buf, lnum)
  local signs = {}

  local extmarks = vim.api.nvim_buf_get_extmarks(
    buf,
    -1,
    { lnum - 1, 0 },
    { lnum - 1, -1 },
    { details = true, type = "sign" }
  )
  for _, extmark in pairs(extmarks) do
    signs[#signs + 1] = {
      name = extmark[4].sign_hl_group or extmark[4].sign_name or "",
      text = extmark[4].sign_text,
      texthl = extmark[4].sign_hl_group,
      priority = extmark[4].priority,
    }
  end

  table.sort(signs, function(a, b)
    return (a.priority or 0) < (b.priority or 0)
  end)

  return signs
end

function M.get_mark(buf, lnum)
  local marks = vim.fn.getmarklist(buf)
  vim.list_extend(marks, vim.fn.getmarklist())
  for _, mark in ipairs(marks) do
    if mark.pos[1] == buf and mark.pos[2] == lnum and mark.mark:match("[a-zA-Z]") then
      return { text = mark.mark:sub(2), texthl = "DiagnosticHint" }
    end
  end
end

function M.icon(sign, len)
  sign = sign or {}
  len = len or 2
  local text = vim.fn.strcharpart(sign.text or "", 0, len)
  text = text .. string.rep(" ", len - vim.fn.strchars(text))
  return sign.texthl and ("%#" .. sign.texthl .. "#" .. text .. "%*") or text
end

function M.click_args(component, minwid, clicks, button, mods)
  local args = {
    minwid = minwid,
    clicks = clicks,
    button = button,
    mods = mods,
    mousepos = vim.fn.getmousepos(),
  }
  if not component.signs then
    component.signs = {}
  end
  args.char = vim.fn.screenstring(args.mousepos.screenrow, args.mousepos.screencol)
  if args.char == " " then
    args.char = vim.fn.screenstring(args.mousepos.screenrow, args.mousepos.screencol - 1)
  end
  args.sign = component.signs[args.char]
  if not args.sign then
    for _, sign_def in ipairs(vim.fn.sign_getdefined()) do
      if sign_def.text then
        component.signs[sign_def.text:gsub("%s", "")] = sign_def
      end
    end
    args.sign = component.signs[args.char]
  end
  vim.api.nvim_set_current_win(args.mousepos.winid)
  vim.api.nvim_win_set_cursor(0, { args.mousepos.line, 0 })
  return args
end

function M.gitsigns_click_handler(_)
  local gitsigns_available, gitsigns = pcall(require, "gitsigns")
  if gitsigns_available then
    vim.schedule(gitsigns.preview_hunk)
  end
end

function M.diagnostics_click_handler(args)
  if args.mods:find("c") then
    vim.schedule(vim.lsp.buf.code_action)
  else
    vim.schedule(vim.diagnostic.open_float)
  end
end

function M.dap_breakpoint_click_handler(_)
  local dap_available, dap = pcall(require, "dap")
  if dap_available then
    vim.schedule(dap.toggle_breakpoint)
  end
end

return M
