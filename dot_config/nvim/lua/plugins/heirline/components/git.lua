local conditions = require("heirline.conditions")
local uplugin = require("util.plugin")

return {
  condition = conditions.is_git_repo,
  init = function(self)
    ---@diagnostic disable-next-line: undefined-field
    self.status_dict = vim.b.gitsigns_status_dict
    self.has_changes = self.status_dict.added ~= 0 or self.status_dict.removed ~= 0 or self.status_dict.changed ~= 0
  end,
  on_click = {
    callback = function()
      if uplugin.has("telescope.nvim") then
        vim.defer_fn(function()
          require("telescope.builtin").git_branches({ use_file_path = true })
        end, 100)
      end
    end,
    name = "heirline_git",
    update = false,
  },
  hl = { fg = "orange" },
  {
    provider = function(self)
      return self.status_dict.head and (" " .. self.status_dict.head .. " ")
    end,
    hl = { bold = true },
  },
  {
    condition = function(self)
      return self.has_changes
    end,
    provider = "(",
  },
  {
    provider = function(self)
      local count = self.status_dict.added or 0
      return count > 0 and ("+" .. count)
    end,
    hl = "diffAdded",
  },
  {
    provider = function(self)
      local count = self.status_dict.removed or 0
      return count > 0 and ("-" .. count)
    end,
    hl = "diffDeleted",
  },
  {
    provider = function(self)
      local count = self.status_dict.changed or 0
      return count > 0 and ("~" .. count)
    end,
    hl = "diffChanged",
  },
  {
    condition = function(self)
      return self.has_changes
    end,
    provider = ") ",
  },
}
