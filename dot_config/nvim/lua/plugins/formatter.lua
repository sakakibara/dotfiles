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
