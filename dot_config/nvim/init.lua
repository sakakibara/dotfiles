local g = vim.g
local v = vim.v
local o = vim.opt
local cmd = vim.cmd
local map = vim.keymap
local fn = vim.fn

g['loaded_gzip'] = 1
g['loaded_man'] = 1
g['loaded_matchit'] = 1
g['loaded_matchparen'] = 1
g['loaded_shada_plugin'] = 1
g['loaded_tarPlugin'] = 1
g['loaded_tar'] = 1
g['loaded_zipPlugin'] = 1
g['loaded_zip'] = 1
g['loaded_netrwPlugin'] = 1

g.mapleader = ' '

require('plugins')

-- Enable mouse
o.mouse = 'a'

-- Disable swap file attention message
o.shortmess:append('A')

-- Disable startup screen message
o.shortmess:append('I')

-- Use system clipboard by default
o.clipboard:prepend('unnamed')

-- Disable redraw while executing macros
o.lazyredraw = true

-- Default tab settings
o.tabstop = 2
o.softtabstop = -1
o.shiftwidth = 0
o.expandtab = true

-- Searching
o.ignorecase = true
o.smartcase = true

-- Case insensitive file name completion
o.wildignorecase = true

-- Scroll offset
o.scrolloff = 5

-- Use persistent undo
o.undofile = true

-- Line numbers
o.number = true
o.relativenumber = true

-- Color theme
o.termguicolors = true
g.colors_name = 'catppuccin'
o.background = o.background

-- Mappings
map.set('i', 'jk', '<Esc>')
map.set('n', '<Leader>w', '<C-w>')

vim.api.nvim_create_autocmd('BufWritePre', {
  callback = function ()
    require('mkdir').run(fn.expand('<afile>:p:h'), v.cmdbang)
  end,
})
