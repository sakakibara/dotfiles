return {
  {
    import = "plugins.extras.linter.eslint",
    enabled = function()
      return vim.fn.executable("node")
    end,
  },
  {
    import = "plugins.extras.linter.typos",
    enabled = function()
      return vim.fn.executable("typos")
    end,
  },
}
