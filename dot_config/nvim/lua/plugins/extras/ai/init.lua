local is_private = Util.system.role() == "private"

return {
  {
    import = "plugins.extras.ai.codeium",
    enabled = function()
      return is_private
    end,
  },
  {
    import = "plugins.extras.ai.copilot",
    enabled = function()
      return not is_private
    end,
  },
  {
    import = "plugins.extras.ai.copilot-chat",
    enabled = function()
      return not is_private
    end,
  },
}
