-- lua/config/plugins/lang/git.lua
if vim.fn.executable("git") == 0 then return {} end

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({
    "git_config", "gitcommit", "git_rebase", "gitignore", "gitattributes",
  })
end)

-- Note: old config had `petertriho/cmp-git` (nvim-cmp source). Skipped — M2 uses
-- blink.cmp and there is no direct blink-native equivalent yet. Revisit later.

return {}
