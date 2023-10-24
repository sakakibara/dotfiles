local icons = require("config.icons")
local upath = require("util.path")

return {
  init = function(self)
    local file_path = self.file_path or upath.get_current_file_path()
    if upath.is_dir(file_path) then
      self.icon, self.icon_color = icons.status.DirectoryAlt, "blue"
    else
      local extension = vim.fn.fnamemodify(file_path, ":e")
      self.icon, self.icon_color = require("nvim-web-devicons").get_icon_color(file_path, extension, { default = true })
      self.icon = self.icon and self.icon .. " "
    end
  end,
  provider = function(self)
    return self.icon
  end,
  hl = function(self)
    return { fg = self.icon_color }
  end,
  update = { "BufEnter", "BufWritePost" },
}
