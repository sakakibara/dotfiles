return {
  {
    import = "plugins.extras.linter.eslint",
    enabled = function()
      return vim.fn.executable("node")
    end,
  },
}
