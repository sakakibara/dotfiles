return {
  {
    "nvim-neotest/neotest",
    opts = {
      adapters = {},
      status = { virtual_text = true },
      output = { open_on_run = true },
      quickfix = {
        open = function()
          if Util.plugin.has("trouble.nvim") then
            require("trouble").open({ mode = "quickfix", focus = false })
          else
            vim.cmd("copen")
          end
        end,
      },
    },
    config = function(_, opts)
      local neotest_ns = vim.api.nvim_create_namespace("neotest")
      vim.diagnostic.config({
        virtual_text = {
          format = function(diagnostic)
            local message = diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
            return message
          end,
        },
      }, neotest_ns)

      if Util.plugin.has("trouble.nvim") then
        opts.consumers = opts.consumers or {}
        opts.consumers.trouble = function(client)
          client.listeners.results = function(adapter_id, results, partial)
            if partial then
              return
            end
            local tree = assert(client:get_position(nil, { adapter = adapter_id }))

            local failed = 0
            for pos_id, result in pairs(results) do
              if result.status == "failed" and tree:get_key(pos_id) then
                failed = failed + 1
              end
            end
            vim.schedule(function()
              local trouble = require("trouble")
              if trouble.is_open() then
                trouble.refresh()
                if failed == 0 then
                  trouble.close()
                end
              end
            end)
            return {}
          end
        end
      end

      if opts.adapters then
        local adapters = {}
        for name, config in pairs(opts.adapters or {}) do
          if type(name) == "number" then
            if type(config) == "string" then
              config = require(config)
            end
            adapters[#adapters + 1] = config
          elseif config ~= false then
            local adapter = require(name)
            if type(config) == "table" and not vim.tbl_isempty(config) then
              local meta = getmetatable(adapter)
              if adapter.setup then
                adapter.setup(config)
              elseif meta and meta.__call then
                adapter(config)
              else
                error("Adapter " .. name .. " does not support setup")
              end
            end
            adapters[#adapters + 1] = adapter
          end
        end
        opts.adapters = adapters
      end

      require("neotest").setup(opts)
    end,
    keys = {
      {
        "<Leader>tt",
        function()
          require("neotest").run.run(vim.fn.expand("%"))
        end,
        desc = "Run File",
      },
      {
        "<Leader>tT",
        function()
          require("neotest").run.run(vim.uv.cwd())
        end,
        desc = "Run All Test Files",
      },
      {
        "<Leader>tr",
        function()
          require("neotest").run.run()
        end,
        desc = "Run Nearest",
      },
      {
        "<Leader>ts",
        function()
          require("neotest").summary.toggle()
        end,
        desc = "Toggle Summary",
      },
      {
        "<Leader>to",
        function()
          require("neotest").output.open({ enter = true, auto_close = true })
        end,
        desc = "Show Output",
      },
      {
        "<Leader>tO",
        function()
          require("neotest").output_panel.toggle()
        end,
        desc = "Toggle Output Panel",
      },
      {
        "<Leader>tS",
        function()
          require("neotest").run.stop()
        end,
        desc = "Stop",
      },
    },
  },

  {
    "mfussenegger/nvim-dap",
    optional = true,
    keys = {
      {
        "<Leader>td",
        function()
          require("neotest").run.run({ strategy = "dap" })
        end,
        desc = "Debug Nearest",
      },
    },
  },
}
