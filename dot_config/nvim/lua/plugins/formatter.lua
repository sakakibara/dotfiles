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
            require("conform").format({ bufnr = buf })
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
    opts = {
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
    },
    config = function(_, opts)
      opts.formatters = opts.formatters or {}
      for n, f in pairs(opts.formatters) do
        if type(f) == "table" then
          local ok, defaults = pcall(require, "conform.formatters." .. n)
          if ok and type(defaults) == "table" then
            opts.formatters[n] = vim.tbl_deep_extend("force", {}, defaults, f)
          end
        end
      end
      require("conform").setup(opts)
    end,
  },
}
