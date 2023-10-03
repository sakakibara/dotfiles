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
      require("util.lazy").on_very_lazy(function()
        require("plugins.lsp.format").custom_format = function(buf)
          return require("conform").format({ bufnr = buf })
        end
      end)
    end,
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        fish = { "fish_indent" },
        sh = { "shfmt" },
      },
      formatters = {
        dprint = {
          condition = function(ctx)
            return vim.fs.find({ "dprint.json" }, { path = ctx.filename, upward = true })[1]
          end,
        },
      },
    },
    config = function(_, opts)
      opts.formatters = opts.formatters or {}
      for f, o in pairs(opts.formatters) do
        local ok, formatter = pcall(require, "conform.formatters." .. f)
        opts.formatters[f] = vim.tbl_deep_extend("force", {}, ok and formatter or {}, o)
      end
      require("conform").setup(opts)
    end,
  },
}
