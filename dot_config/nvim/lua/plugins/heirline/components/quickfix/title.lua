return {
  fallthrough = false,
  {
    condition = require("plugins.heirline.components.quickfix.condition").is_loclist,
    provider = function()
      return vim.fn.getloclist(0, { title = 0 }).title
    end,
  },
  {
    provider = function()
      return vim.fn.getqflist({ title = 0 }).title
    end,
  },
}
