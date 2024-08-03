local conditions = require("heirline.conditions")

return {
  condition = function()
    return conditions.buffer_matches({
      filetype = { "dashboard" },
    })
  end,
  {
    init = function(self)
      self.fpath = Util.path.get_current_file_path()
      self.pwd = vim.uv.cwd()
    end,
    require("plugins.heirline.components.file.work_dir"),
  },
  require("plugins.heirline.components.align"),
  require("plugins.heirline.components.file.type"),
}
