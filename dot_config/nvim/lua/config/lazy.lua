return {
  spec = {
    { import = "plugins" },
    { import = "plugins.extras" },
  },
  concurrency = vim.uv.available_parallelism() * 14,
  install = { colorscheme = { "evergarden" } },
  performance = {
    cache = {
      enabled = true,
    },
    rtp = {
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
}
