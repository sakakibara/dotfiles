local LazyUtil = require("lazy.core.util")

local M = {}

local format_opts = {}

function M.setup(_, opts)
  local util = require("conform.util")
  opts.formatters = opts.formatters or {}
  for name, formatter in pairs(opts.formatters) do
    if type(formatter) == "table" then
      local ok, defaults = pcall(require, "conform.formatters." .. name)
      if ok and type(defaults) == "table" then
        opts.formatters[name] = vim.tbl_deep_extend("force", {}, defaults, formatter)
      end
      if opts.formatters[name].extra_args then
        opts.formatters[name].args =
          util.extend_args(opts.formatters[name].args or {}, opts.formatters[name].extra_args)
      end
    end
  end

  for _, key in ipairs({ "format_on_save", "format_after_save" }) do
    if opts[key] then
      LazyUtil.warn(("Setting `opts.%s` for `conform.nvim` is not supported."):format(key))
      opts[key] = nil
    end
  end
  format_opts = opts.format
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
        "<leader>cF",
        function()
          require("conform").format({ formatters = { "injected" } })
        end,
        mode = { "n", "v" },
        desc = "Format treesitter injected languages",
      },
    },
    init = function()
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
      require("util.plugin").on_very_lazy(function()
        require("util.format").register({
          name = "conform.nvim",
          priority = 100,
          primary = true,
          format = function(buf)
            require("conform").format(LazyUtil.merge(format_opts, { bufnr = buf }))
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
        LazyUtil.error({
          "Don't set `plugin.config` for `conform.nvim`.",
        }, { title = "Core" })
      end
      return {
        format = {
          timeout_ms = 1000,
        },
        formatters_by_ft = {
          lua = { "stylua" },
          fish = { "fish_indent" },
          sh = { "shfmt" },
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
    end,
    config = M.setup,
  },
}
