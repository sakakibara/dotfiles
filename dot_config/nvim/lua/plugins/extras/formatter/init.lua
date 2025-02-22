return {
  {
    import = "plugins.extras.formatter.black",
    enabled = function()
      return vim.fn.executable("python3") == 1
    end,
  },
  {
    import = "plugins.extras.formatter.prettier",
    enabled = function()
      return vim.fn.executable("node") == 1
    end,
  },
}
