local icons = require("config.icons")
local uplugin = require("util.plugin")

return {
  condition = function()
    local session

    if uplugin.has("dap") then
      session = require("dap").session()
    end

    return session ~= nil
  end,
  provider = function()
    return icons.status.Debug .. require("dap").status() .. " "
  end,
  hl = "Debug",
  {
    provider = " ",
    on_click = {
      callback = function()
        require("dap").step_into()
      end,
      name = "heirline_dap_step_into",
    },
  },
  { provider = " " },
  {
    provider = " ",
    on_click = {
      callback = function()
        require("dap").step_out()
      end,
      name = "heirline_dap_step_out",
    },
  },
  { provider = " " },
  {
    provider = " ",
    on_click = {
      callback = function()
        require("dap").step_over()
      end,
      name = "heirline_dap_step_over",
    },
  },
  { provider = " " },
  {
    provider = " ",
    hl = { fg = "green" },
    on_click = {
      callback = function()
        require("dap").run_last()
      end,
      name = "heirline_dap_run_last",
    },
  },
  { provider = " " },
  {
    provider = " ",
    hl = { fg = "red" },
    on_click = {
      callback = function()
        require("dap").terminate()
        require("dapui").close({})
      end,
      name = "heirline_dap_close",
    },
  },
  { provider = " " },
}
