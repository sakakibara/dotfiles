local builder = require("plugins.heirline.components.nav.builder")
local utils = require("heirline.utils")

return {
  condifion = function()
    return package.loaded["aerial"]
  end,
  static = {
    type_hl = {
      File = Util.ui.dim(utils.get_highlight("Directory").fg, 0.75),
      Module = Util.ui.dim(utils.get_highlight("@include").fg, 0.75),
      Namespace = Util.ui.dim(utils.get_highlight("@namespace").fg, 0.75),
      Package = Util.ui.dim(utils.get_highlight("@include").fg, 0.75),
      Class = Util.ui.dim(utils.get_highlight("@type").fg, 0.75),
      Method = Util.ui.dim(utils.get_highlight("@method").fg, 0.75),
      Property = Util.ui.dim(utils.get_highlight("@property").fg, 0.75),
      Field = Util.ui.dim(utils.get_highlight("@field").fg, 0.75),
      Constructor = Util.ui.dim(utils.get_highlight("@constructor").fg, 0.75),
      Enum = Util.ui.dim(utils.get_highlight("@type").fg, 0.75),
      Interface = Util.ui.dim(utils.get_highlight("@type").fg, 0.75),
      Function = Util.ui.dim(utils.get_highlight("@function").fg, 0.75),
      Variable = Util.ui.dim(utils.get_highlight("@variable").fg, 0.75),
      Constant = Util.ui.dim(utils.get_highlight("@constant").fg, 0.75),
      String = Util.ui.dim(utils.get_highlight("@string").fg, 0.75),
      Number = Util.ui.dim(utils.get_highlight("@number").fg, 0.75),
      Boolean = Util.ui.dim(utils.get_highlight("@boolean").fg, 0.75),
      Array = Util.ui.dim(utils.get_highlight("@field").fg, 0.75),
      Object = Util.ui.dim(utils.get_highlight("@type").fg, 0.75),
      Key = Util.ui.dim(utils.get_highlight("@keyword").fg, 0.75),
      Null = Util.ui.dim(utils.get_highlight("@comment").fg, 0.75),
      EnumMember = Util.ui.dim(utils.get_highlight("@constant").fg, 0.75),
      Struct = Util.ui.dim(utils.get_highlight("@type").fg, 0.75),
      Event = Util.ui.dim(utils.get_highlight("@type").fg, 0.75),
      Operator = Util.ui.dim(utils.get_highlight("@operator").fg, 0.75),
      TypeParameter = Util.ui.dim(utils.get_highlight("@type").fg, 0.75),
    },
    enc = function(line, col, winnr)
      return bit.bor(bit.lshift(line, 16), bit.lshift(col, 6), winnr)
    end,
    dec = function(c)
      local line = bit.rshift(c, 16)
      local col = bit.band(bit.rshift(c, 6), 1023)
      local winnr = bit.band(c, 63)
      return line, col, winnr
    end,
  },
  init = function(self)
    self.data = require("aerial").get_location(true)
  end,
  update = {
    "CursorMoved",
    "WinResized",
  },
  hl = { fg = "gray" },
  flexible = 50,
  {
    init = function(self)
      builder.symbol_segments({ prefix = true })(self)
    end,
  },
  {
    init = function(self)
      builder.symbol_segments({
        prefix = true,
        max_depth = 3,
      })(self)
    end,
  },
  { provider = "" },
}
