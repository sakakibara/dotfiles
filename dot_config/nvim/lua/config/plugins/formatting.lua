-- lua/config/plugins/formatting.lua
return {
  {
    "stevearc/conform.nvim",
    name = "conform.nvim",
    event = { "BufWritePre" },
    cmd = "ConformInfo",
    init = function()
      Lib.mason.add("stylua")
    end,
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
      },
      default_format_opts = {
        timeout_ms = 1000,
        lsp_format = "fallback",
      },
      formatters = {
        stylua = {},
      },
    },
  },

  {
    "mfussenegger/nvim-lint",
    name = "nvim-lint",
    event = "LazyFile",
    init = function()
      Lib.mason.add("selene")
    end,
    config = function()
      local lint = require("lint")
      lint.linters_by_ft = {
        lua = { "selene" },
      }
      local grp = vim.api.nvim_create_augroup("Lib.lint", { clear = true })
      local timer = assert(vim.uv.new_timer())
      local debounce_ms = 200
      vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost", "InsertLeave" }, {
        group = grp,
        callback = function(args)
          timer:stop()
          timer:start(debounce_ms, 0, vim.schedule_wrap(function()
            local ft = vim.bo[args.buf].filetype
            local configured = lint.linters_by_ft[ft] or {}
            local runnable = {}
            for _, name in ipairs(configured) do
              -- Resolve the binary: prefer linter.cmd if it's a static string,
              -- else fall back to the linter name. (Don't invoke function cmds
              -- speculatively — they may have side effects.)
              local linter = lint.linters[name]
              local bin = (type(linter) == "table" and type(linter.cmd) == "string")
                and linter.cmd
                or name
              if vim.fn.executable(bin) == 1 then
                runnable[#runnable + 1] = name
              end
            end
            if #runnable > 0 then
              lint.try_lint(runnable)
            end
          end))
        end,
      })
    end,
  },
}
