-- ~/.config/nvim/init.lua
vim.loader.enable()

-- Disable built-in plugins we don't use, before runtime sourcing scans them.
-- Each save is small (~0.05–0.6 ms); cumulatively ~1 ms off cold start.
do
  local builtins = {
    "netrwPlugin",   -- using oil.nvim
    "tarPlugin",     -- no .tar editing
    "zipPlugin",     -- no .zip editing
    "tutor",         -- no :Tutor usage
    "rplugin",       -- no remote plugins (nvim_rplugin)
    "matchit",       -- using vim-matchup
    "man",           -- prefer LSP hover / browser docs
  }
  for _, name in ipairs(builtins) do
    vim.g["loaded_" .. name] = 1
  end
end

-- Disable language providers we don't use. nvim probes for python3/ruby/perl/
-- node interpreters on startup unless these are set; we have no plugins that
-- need them, so the probes are pure waste.
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider    = 0
vim.g.loaded_perl_provider    = 0
vim.g.loaded_node_provider    = 0

require("config").setup()
