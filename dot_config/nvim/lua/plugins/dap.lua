local function get_args(config)
  local args = type(config.args) == "function" and (config.args() or {}) or config.args or {}
  config = vim.deepcopy(config)
  ---@cast args string[]
  config.args = function()
    local new_args = vim.fn.input("Run with args: ", table.concat(args, " "))
    return vim.split(vim.fn.expand(new_args), " ")
  end
  return config
end

return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      {
        "rcarriga/nvim-dap-ui",
        dependencies = { "nvim-neotest/nvim-nio" },
        keys = {
          {
            "<Leader>de",
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
        "jbyuki/one-small-step-for-vimkind",
        config = function()
          local dap = require("dap")
          dap.adapters.nlua = function(callback, conf)
            local adapter = {
              type = "server",
              host = conf.host or "127.0.0.1",
              port = conf.port or 8086,
            }
            if conf.start_neovim then
              local dap_run = dap.run
              dap.run = function(c)
                adapter.port = c.port
                adapter.host = c.host
              end
              require("osv").run_this()
              dap.run = dap_run
            end
            callback(adapter)
          end
          dap.configurations.lua = {
            {
              type = "nlua",
              request = "attach",
              name = "Run this file",
              start_neovim = {},
            },
            {
              type = "nlua",
              request = "attach",
              name = "Attach to running Neovim instance (port = 8086)",
              port = 8086,
            },
          }
        end,
      },
    },
    keys = {
      {
        "<Leader>dB",
        function()
          require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
        end,
        desc = "Breakpoint condition",
      },
      {
        "<Leader>db",
        function()
          require("dap").toggle_breakpoint()
        end,
        desc = "Toggle breakpoint",
      },
      {
        "<Leader>dc",
        function()
          require("dap").continue()
        end,
        desc = "Continue",
      },
      {
        "<Leader>da",
        function()
          require("dap").continue({ before = get_args })
        end,
        desc = "Run with args",
      },
      {
        "<Leader>dC",
        function()
          require("dap").run_to_cursor()
        end,
        desc = "Run to cursor",
      },
      {
        "<Leader>dg",
        function()
          require("dap").goto_()
        end,
        desc = "Go to line (no execute)",
      },
      {
        "<Leader>di",
        function()
          require("dap").step_into()
        end,
        desc = "Step into",
      },
      {
        "<Leader>dj",
        function()
          require("dap").down()
        end,
        desc = "Down",
      },
      {
        "<Leader>dk",
        function()
          require("dap").up()
        end,
        desc = "Up",
      },
      {
        "<Leader>dl",
        function()
          require("dap").run_last()
        end,
        desc = "Run last",
      },
      {
        "<Leader>do",
        function()
          require("dap").step_over()
        end,
        desc = "Step over",
      },
      {
        "<Leader>dO",
        function()
          require("dap").step_out()
        end,
        desc = "Step out",
      },
      {
        "<Leader>dp",
        function()
          require("dap").pause()
        end,
        desc = "Pause",
      },
      {
        "<Leader>dr",
        function()
          require("dap").repl.toggle()
        end,
        desc = "Toggle REPL",
      },
      {
        "<Leader>ds",
        function()
          require("dap").session()
        end,
        desc = "Session",
      },
      {
        "<Leader>dt",
        function()
          require("dap").terminate()
        end,
        desc = "Terminate",
      },
      {
        "<Leader>dw",
        function()
          require("dap.ui.widgets").hover()
        end,
        desc = "Widgets",
      },
    },
    config = function()
      vim.api.nvim_set_hl(0, "DapStoppedLine", { default = true, link = "Visual" })

      for name, sign in pairs(Util.config.icons.dap) do
        sign = type(sign) == "table" and sign or { sign }
        vim.fn.sign_define(
          "Dap" .. name,
          { text = sign[1], texthl = sign[2] or "DiagnosticInfo", linehl = sign[3], numhl = sign[3] }
        )
      end
      local vscode = require("dap.ext.vscode")
      local json = require("plenary.json")
      vscode.json_decode = function(str)
        return vim.json.decode(json.json_strip_comments(str))
      end
    end,
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
}
