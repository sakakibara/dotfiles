return {
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = "ConformInfo",
    init = function()
      Lib.mason.add("stylua", { ft = "lua" })
      -- Register as a primary format source. Done in init (pre-load) so
      -- Lib.format.resolve sees conform before the first BufWritePre fires.
      -- sources() uses list_formatters (configured, regardless of binary
      -- availability) — conform.format() itself will surface the real
      -- error if the binary is missing.
      Lib.format.register({
        name     = "conform",
        primary  = true,
        priority = 100,
        format   = function(buf)
          require("conform").format({ bufnr = buf, timeout_ms = 1000 })
        end,
        sources  = function(buf)
          local ok, conform = pcall(require, "conform")
          if not ok then return {} end
          local names = {}
          for _, f in ipairs(conform.list_formatters(buf)) do
            names[#names + 1] = f.name
          end
          return names
        end,
      })
    end,
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        org = { "organ" },
      },
      default_format_opts = {
        timeout_ms = 1000,
        lsp_format = "fallback",
      },
      formatters = {
        stylua = {},
        organ = {
          format = function(_, ctx, lines, callback)
            local ok, out = pcall(require("organ.format").format_lines, lines, nil, ctx.buf)
            if not ok then
              callback(out, nil)
              return
            end
            callback(nil, out)
          end,
        },
      },
    },
  },

  {
    "mfussenegger/nvim-lint",
    event = "LazyFile",
    config = function()
      local lint = require("lint")
      -- lua linting is owned by lua_ls (sumneko); a second linter
      -- (selene) duplicates work and complains about `vim` being
      -- undefined unless given a custom selene.toml + nvim std lib.
      lint.linters_by_ft = {}
      local grp = vim.api.nvim_create_augroup("Lib.lint", { clear = true })
      local debounce_ms = 200

      -- Per-buffer timers: saves to different buffers in quick succession
      -- must each get linted (a single shared timer would cancel the earlier
      -- one). try_lint reads the current buffer, so we run it via
      -- nvim_buf_call to target the buffer that actually triggered the
      -- event — even if focus has moved since.
      local timers = {} ---@type table<integer, uv.uv_timer_t>
      local function cleanup(buf)
        local t = timers[buf]
        if t then
          t:stop(); t:close()
          timers[buf] = nil
        end
      end

      vim.api.nvim_create_autocmd("BufWipeout", {
        group = grp,
        callback = function(args) cleanup(args.buf) end,
      })

      vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost", "InsertLeave" }, {
        group = grp,
        callback = function(args)
          local buf = args.buf
          local t = timers[buf]
          if not t then
            t = assert(vim.uv.new_timer())
            timers[buf] = t
          end
          t:stop()
          t:start(debounce_ms, 0, vim.schedule_wrap(function()
            if not vim.api.nvim_buf_is_valid(buf) then
              cleanup(buf)
              return
            end
            local ft = vim.bo[buf].filetype
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
              vim.api.nvim_buf_call(buf, function() lint.try_lint(runnable) end)
            end
          end))
        end,
      })
    end,
  },
}
