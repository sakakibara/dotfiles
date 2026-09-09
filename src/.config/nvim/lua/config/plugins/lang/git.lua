vim.treesitter.language.register("git_config", { "gitconfig" })
vim.treesitter.language.register("git_rebase", { "gitrebase" })

-- No completion source for git refs and issues: blink.cmp has no equivalent
-- of nvim-cmp's cmp-git.
return Lib.lang.setup({
  cmd = "git",
  ft = { "gitcommit", "gitrebase", "gitconfig", "gitattributes", "gitignore" },
  parsers = {
    "git_config", "gitcommit", "git_rebase", "gitignore", "gitattributes",
  },
})
