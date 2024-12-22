return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "git_config", "gitcommit", "git_rebase", "gitignore", "gitattributes" } },
  },

  {
    "hrsh7th/nvim-cmp",
    optional = true,
    dependencies = {
      { "petertriho/cmp-git", opts = {} },
    },
    opts = function(_, opts)
      table.insert(opts.sources, { name = "git" })
    end,
  },
}
