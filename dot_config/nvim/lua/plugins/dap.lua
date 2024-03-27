return {
  "mfussenegger/nvim-dap",
  dependencies = {
    {
      "rcarriga/nvim-dap-ui",
      dependencies = { "nvim-neotest/nvim-nio" },
      keys = {
        {
          "<leader>de",
          function()
            require("dapui").eval()
          end,
          desc = "Eval",
          mode = { "n", "v" },
        },
      },
      opts = {},
      config = function(_, opts)
        local dap = require("dap")
        local dapui = require("dapui")
        dapui.setup(opts)

        local debug_win = nil
        local debug_tab = nil
        local debug_tabnr = nil

        local function open_in_tab()
          if debug_win and vim.api.nvim_win_is_valid(debug_win) then
            vim.api.nvim_set_current_win(debug_win)
            return
          end

          vim.api.nvim_command("tabedit %")
          debug_win = vim.fn.win_getid()
          debug_tab = vim.api.nvim_win_get_tabpage(debug_win)
          debug_tabnr = vim.api.nvim_tabpage_get_number(debug_tab)

          dapui.open()
        end

        local function close_tab()
          dapui.close()

          if debug_tab and vim.api.nvim_tabpage_is_valid(debug_tab) then
            vim.api.nvim_command("tabclose " .. debug_tabnr)
          end

          debug_win = nil
          debug_tab = nil
          debug_tabnr = nil
        end

        dap.listeners.after.event_initialized["dapui_config"] = function()
          open_in_tab()
        end
        dap.listeners.before.event_terminated["dapui_config"] = function()
          close_tab()
        end
        dap.listeners.before.event_exited["dapui_config"] = function()
          close_tab()
        end
      end,
    },
    {
      "theHamsta/nvim-dap-virtual-text",
      opts = {},
    },
    {
      "jay-babu/mason-nvim-dap.nvim",
      dependencies = "mason.nvim",
      cmd = { "DapInstall", "DapUninstall" },
      opts = {
        automatic_installation = true,
        handlers = {},
        ensure_installed = {},
      },
    },
    {
      "jbyuki/one-small-step-for-vimkind",
      keys = {
        {
          "<leader>daL",
          function()
            require("osv").launch({ port = 8086 })
          end,
          desc = "Adapter lua server",
        },
        {
          "<leader>dal",
          function()
            require("osv").run_this()
          end,
          desc = "Adapter lua",
        },
      },
      config = function()
        local dap = require("dap")
        dap.adapters.nlua = function(callback, config)
          ---@diagnostic disable-next-line: undefined-field
          callback({ type = "server", host = config.host or "127.0.0.1", port = config.port or 8086 })
        end
        dap.configurations.lua = {
          {
            type = "nlua",
            request = "attach",
            name = "Attach to running Neovim instance",
          },
        }
      end,
    },
  },
  keys = {
    {
      "<leader>dB",
      function()
        require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
      end,
      desc = "Breakpoint condition",
    },
    {
      "<leader>db",
      function()
        require("dap").toggle_breakpoint()
      end,
      desc = "Toggle breakpoint",
    },
    {
      "<leader>dc",
      function()
        require("dap").continue()
      end,
      desc = "Continue",
    },
    {
      "<leader>dC",
      function()
        require("dap").run_to_cursor()
      end,
      desc = "Run to cursor",
    },
    {
      "<leader>dg",
      function()
        require("dap").goto_()
      end,
      desc = "Go to line (no execute)",
    },
    {
      "<leader>di",
      function()
        require("dap").step_into()
      end,
      desc = "Step into",
    },
    {
      "<leader>dj",
      function()
        require("dap").down()
      end,
      desc = "Down",
    },
    {
      "<leader>dk",
      function()
        require("dap").up()
      end,
      desc = "Up",
    },
    {
      "<leader>dl",
      function()
        require("dap").run_last()
      end,
      desc = "Run last",
    },
    {
      "<leader>do",
      function()
        require("dap").step_over()
      end,
      desc = "Step over",
    },
    {
      "<leader>dO",
      function()
        require("dap").step_out()
      end,
      desc = "Step out",
    },
    {
      "<leader>dp",
      function()
        require("dap").pause()
      end,
      desc = "Pause",
    },
    {
      "<leader>dr",
      function()
        require("dap").repl.toggle()
      end,
      desc = "Toggle REPL",
    },
    {
      "<leader>ds",
      function()
        require("dap").session()
      end,
      desc = "Session",
    },
    {
      "<leader>dt",
      function()
        require("dap").terminate()
      end,
      desc = "Terminate",
    },
    {
      "<leader>dw",
      function()
        require("dap.ui.widgets").hover()
      end,
      desc = "Widgets",
    },
  },
  config = function()
    local icons = require("config.icons")
    vim.api.nvim_set_hl(0, "DapStoppedLine", { default = true, link = "Visual" })

    for name, sign in pairs(icons.dap) do
      sign = type(sign) == "table" and sign or { sign }
      vim.fn.sign_define(
        "Dap" .. name,
        { text = sign[1], texthl = sign[2] or "DiagnosticInfo", linehl = sign[3], numhl = sign[3] }
      )
    end
  end,
}
