local builder = require("plugins.heirline.components.breadcrumbs.builder")

return {
  flexible = 100,
  {
    init = function(self)
      builder.dir_segments({ suffix = true })(self)
    end,
  },
  {
    init = function(self)
      builder.dir_segments({
        suffix = true,
        max_depth = 3,
      })(self)
    end,
  },
  {
    init = function(self)
      builder.dir_segments({
        suffix = true,
        max_char = 1,
      })(self)
    end,
  },
  {
    init = function(self)
      builder.dir_segments({
        suffix = true,
        max_char = 1,
        max_depth = 3,
      })(self)
    end,
  },
  { provider = "" },
  update = {
    "BufEnter",
    "DirChanged",
  },
}
