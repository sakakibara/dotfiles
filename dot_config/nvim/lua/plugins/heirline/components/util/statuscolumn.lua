local M = {}

function M.clickargs(self, minwid, clicks, button, mods)
  local args = {
    minwid = minwid,
    clicks = clicks,
    button = button,
    mods = mods,
    mousepos = vim.fn.getmousepos(),
  }
  if not self.signs then
    self.signs = {}
  end
  args.char = vim.fn.screenstring(args.mousepos.screenrow, args.mousepos.screencol)
  if args.char == " " then
    args.char = vim.fn.screenstring(args.mousepos.screenrow, args.mousepos.screencol - 1)
  end
  args.sign = self.signs[args.char]
  if not args.sign then
    for _, sign_def in ipairs(vim.fn.sign_getdefined()) do
      if sign_def.text then
        self.signs[sign_def.text:gsub("%s", "")] = sign_def
      end
    end
    args.sign = self.signs[args.char]
  end
  vim.api.nvim_set_current_win(args.mousepos.winid)
  vim.api.nvim_win_set_cursor(0, { args.mousepos.line, 0 })
  return args
end

return M
