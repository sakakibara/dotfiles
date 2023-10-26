return {
  spec = {
    { import = "plugins" },
    { import = "plugins.lang" },
  },
  install = { colorscheme = { "catppuccin" } },
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
