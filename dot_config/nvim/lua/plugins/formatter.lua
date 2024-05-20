local LazyUtil = require("lazy.core.util")

local M = {}

local conform_opts = {}

function M.setup(_, opts)
  for _, key in ipairs({ "format_on_save", "format_after_save" }) do
    if opts[key] then
      LazyUtil.warn(("Setting `opts.%s` for `conform.nvim` is not supported."):format(key))
      opts[key] = nil
    end
  end
  conform_opts = opts
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
            require("conform").format(LazyUtil.merge({
              timeout_ms = conform_opts.format.timeout_ms,
              async = conform_opts.format.async,
              quiet = conform_opts.format.quiet,
            }, { bufnr = buf }))
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
          timeout_ms = 3000,
          async = false,
          quiet = false,
          lsp_fallback = true,
        },
        formatters_by_ft = {
          ["lua"] = { "stylua" },
          ["fish"] = { "fish_indent" },
          ["sh"] = { "shfmt" },
          ["javascript"] = { "prettierd" },
          ["javascriptreact"] = { "prettierd" },
          ["typescript"] = { "prettierd" },
          ["typescriptreact"] = { "prettierd" },
          ["vue"] = { "prettierd" },
          ["css"] = { "prettierd" },
          ["scss"] = { "prettierd" },
          ["less"] = { "prettierd" },
          ["html"] = { "prettierd" },
          ["json"] = { "prettierd" },
          ["jsonc"] = { "prettierd" },
          ["yaml"] = { "prettierd" },
          ["markdown"] = { "prettierd" },
          ["markdown.mdx"] = { "prettierd" },
          ["graphql"] = { "prettierd" },
          ["handlebars"] = { "prettierd" },
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
