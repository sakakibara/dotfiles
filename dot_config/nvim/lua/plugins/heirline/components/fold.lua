local icons = require("config.icons")

return {
  static = {
    fillchars = vim.opt.fillchars:get(),
    ffi = require("util.ffi"),
  },
  condition = function()
    return vim.opt.foldcolumn:get() ~= "0"
  end,
  provider = function(self)
    local foldopen = self.fillchars.foldopen or icons.status.FoldOpen
    local foldclosed = self.fillchars.foldclose or icons.status.FoldClose
    local foldsep = self.fillchars.foldsep or icons.status.FoldSeparator
    local wp = self.ffi.C.find_window_by_handle(0, self.ffi.new("Error"))
    local width = self.ffi.C.compute_foldcolumn(wp, 0)
    local foldinfo = width > 0 and self.ffi.C.fold_info(wp, vim.v.lnum)
      or { start = 0, level = 0, llevel = 0, lines = 0 }
    local str = ""
    if width ~= 0 then
      str = vim.v.relnum > 0 and "%#FoldColumn#" or "%#CursorLineFold#"
      if foldinfo.level == 0 then
        str = str .. (" "):rep(width)
      else
        local closed = foldinfo.lines > 0
        local first_level = foldinfo.level - width - (closed and 1 or 0) + 1
        if first_level < 1 then
          first_level = 1
        end

        for col = 1, width do
          str = str
            .. (
              (vim.v.virtnum ~= 0 and foldsep)
              or ((closed and (col == foldinfo.level or col == width)) and foldclosed)
              or ((foldinfo.start == vim.v.lnum and first_level + col > foldinfo.llevel) and foldopen)
              or foldsep
            )
          if col == foldinfo.level then
            str = str .. (" "):rep(width - col)
            break
          end
        end
      end
    end
    return str .. "%* "
  end,
}
