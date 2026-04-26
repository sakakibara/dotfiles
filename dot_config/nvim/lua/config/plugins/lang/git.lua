-- lua/config/plugins/lang/git.lua
-- Note: old config had `petertriho/cmp-git` (nvim-cmp source). Skipped — M2 uses
-- blink.cmp and there is no direct blink-native equivalent yet. Revisit later.
return Lib.lang.setup({
  cmd = "git",
  parsers = {
    "git_config", "gitcommit", "git_rebase", "gitignore", "gitattributes",
  },
})
