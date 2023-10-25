return {
  fallthrough = false,
  {
    condition = require("plugins.heirline.components.quickfix.condition").is_loclist,
    provider = "Location List",
  },
  { provider = "Quickfix List" },
}
