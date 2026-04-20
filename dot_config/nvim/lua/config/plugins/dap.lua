-- lua/config/plugins/dap.lua
return {
  {
    "mfussenegger/nvim-dap",
    name = "nvim-dap",
    dependencies = { "nvim-dap-ui", "nvim-nio", "nvim-dap-virtual-text" },
    keys = {
      { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle breakpoint" },
      { "<leader>dB", function() require("dap").set_breakpoint(vim.fn.input("Condition: ")) end, desc = "Conditional breakpoint" },
      { "<leader>dc", function() require("dap").continue() end, desc = "Continue" },
      { "<leader>di", function() require("dap").step_into() end, desc = "Step into" },
      { "<leader>do", function() require("dap").step_over() end, desc = "Step over" },
      { "<leader>dO", function() require("dap").step_out() end, desc = "Step out" },
      { "<leader>dr", function() require("dap").restart() end, desc = "Restart" },
      { "<leader>dt", function() require("dap").terminate() end, desc = "Terminate" },
      { "<leader>du", function() require("dapui").toggle() end, desc = "Toggle dap-ui" },
      { "<leader>de", function() require("dapui").eval() end, desc = "Eval", mode = { "n", "x" } },
      { "<leader>dl", function() require("dap").run_last() end, desc = "Run last" },
      { "<F5>",  function() require("dap").continue() end, desc = "DAP continue" },
      { "<F10>", function() require("dap").step_over() end, desc = "DAP step over" },
      { "<F11>", function() require("dap").step_into() end, desc = "DAP step into" },
      { "<F12>", function() require("dap").step_out() end, desc = "DAP step out" },
    },
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")
      dapui.setup({})
      require("nvim-dap-virtual-text").setup({ commented = true })

      dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
      dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
      dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end

      local D = Lib.icons.dap
      vim.fn.sign_define("DapBreakpoint",          { text = D.Breakpoint,          texthl = "DiagnosticError" })
      vim.fn.sign_define("DapBreakpointCondition", { text = D.BreakpointCondition, texthl = "DiagnosticWarn" })
      vim.fn.sign_define("DapLogPoint",            { text = D.LogPoint,            texthl = "DiagnosticInfo" })
      vim.fn.sign_define("DapStopped",             { text = D.Stopped[1],          texthl = D.Stopped[2], linehl = D.Stopped[3] })
      vim.fn.sign_define("DapBreakpointRejected",  { text = D.BreakpointRejected[1], texthl = D.BreakpointRejected[2] })
    end,
  },
  { "rcarriga/nvim-dap-ui", name = "nvim-dap-ui", lazy = true },
  { "nvim-neotest/nvim-nio", name = "nvim-nio", lazy = true },
  { "theHamsta/nvim-dap-virtual-text", name = "nvim-dap-virtual-text", lazy = true },

  -- Lua DAP adapter (let nvim debug nvim plugins/config)
  {
    "jbyuki/one-small-step-for-vimkind",
    name = "one-small-step-for-vimkind",
    dependencies = { "nvim-dap" },
    keys = {
      {
        "<leader>dL",
        function()
          require("osv").launch({ port = 8086 })
          vim.notify("osv listening on port 8086", vim.log.levels.INFO)
        end,
        desc = "Launch Lua DAP server",
      },
      {
        "<leader>dR",
        function() require("osv").run_this() end,
        desc = "Run this Lua file under DAP",
      },
    },
    config = function()
      local dap = require("dap")
      dap.adapters.nlua = function(callback, conf)
        callback({ type = "server", host = conf.host or "127.0.0.1", port = conf.port or 8086 })
      end
      dap.configurations.lua = {
        { type = "nlua", request = "attach", name = "Attach to running nvim", host = "127.0.0.1", port = 8086 },
      }
    end,
  },

  -- mason bridge: `:MasonToolsInstall` picks up DAP adapters registered here
  {
    "jay-babu/mason-nvim-dap.nvim",
    name = "mason-nvim-dap.nvim",
    dependencies = { "nvim-dap" },
    cmd  = { "DapInstall", "DapUninstall" },
    opts = {
      ensure_installed = {},   -- per-language dap.lua files push adapters here
      automatic_installation = false,
      handlers = {},
    },
  },
}
