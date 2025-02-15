local M = {}

function M.setup(_, opts)
  for _, key in ipairs({ "format_on_save", "format_after_save" }) do
    if opts[key] then
      Util.warn(("Setting `opts.%s` for `conform.nvim` is not supported."):format(key))
      opts[key] = nil
    end
  end
  require("conform").setup(opts)
end

return {
  {
    "stevearc/conform.nvim",
    dependencies = { "mason.nvim" },
    lazy = true,
    cmd = "ConformInfo",
    keys = {
      {
        "<Leader>cF",
        function()
          require("conform").format({ formatters = { "injected" }, timeout_ms = 3000 })
        end,
        mode = { "n", "v" },
        desc = "Format Treesitter Injected Languages",
      },
    },
    init = function()
      Util.plugin.on_very_lazy(function()
        Util.format.register({
          name = "conform.nvim",
          priority = 100,
          primary = true,
          format = function(buf)
            local opts = Util.plugin.opts("conform.nvim")
            require("conform").format(Util.merge({}, opts.format, { bufnr = buf }))
          end,
          sources = function(buf)
            local ret = require("conform").list_formatters(buf)
            return vim.tbl_map(function(v)
              return v.name
            end, ret)
          end,
        })
      end)
    end,
    opts = function()
      local plugin = require("lazy.core.config").plugins["conform.nvim"]
      if plugin.config ~= M.setup then
        Util.error({
          "Don't set `plugin.config` for `conform.nvim`.",
        }, { title = "Core" })
      end
      local opts = {
        format = {
          timeout_ms = 3000,
          async = false,
          quiet = false,
          lsp_fallback = true,
        },
        formatters_by_ft = {
          ["lua"] = { "stylua" },
          ["fish"] = { "fish_indent" },
          ["sh"] = { "shfmt" },
        },
        formatters = {
          injected = { options = { ignore_errors = true } },
          dprint = {
            condition = function(ctx)
              return vim.fs.find({ "dprint.json" }, { path = ctx.filename, upward = true })[1]
            end,
          },
        },
      }
      return opts
    end,
    config = M.setup,
  },
}
