local icons = require("config.icons")

return {
  condition = function()
    local ok, _ = pcall(require, "overseer")
    if ok then
      return true
    end
  end,
  init = function(self)
    self.overseer = require("overseer")
    self.tasks = self.overseer.task_list
    self.STATUS = self.overseer.constants.STATUS
  end,
  static = {
    symbols = {
      ["FAILURE"] = icons.status.Failure,
      ["CANCELED"] = icons.status.Canceled,
      ["SUCCESS"] = icons.status.Success,
      ["RUNNING"] = icons.status.Running,
    },
    colors = {
      ["FAILURE"] = "red",
      ["CANCELED"] = "gray",
      ["SUCCESS"] = "green",
      ["RUNNING"] = "yellow",
    },
  },
  {
    condition = function(self)
      return #self.tasks.list_tasks() > 0
    end,
    {
      provider = function(self)
        local tasks_by_status = self.overseer.util.tbl_group_by(self.tasks.list_tasks({ unique = true }), "status")

        for _, status in ipairs(self.STATUS.values) do
          local status_tasks = tasks_by_status[status]
          if self.symbols[status] and status_tasks then
            self.color = self.colors[status]
            return self.symbols[status] .. " "
          end
        end
      end,
      hl = function(self)
        return { fg = self.color }
      end,
    },
  },
}
