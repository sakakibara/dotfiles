vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Encodings
vim.opt.fileencodings = { "ucs-bom", "utf-8", "iso-2022-jp", "cp932", "euc-jp", "latin1" }
vim.opt.fileformats = { "unix", "dos", "mac" }

-- Enable mouse
vim.opt.mouse = "a"

-- Disable swap file attention message
vim.opt.shortmess:append("A")

-- Disable startup screen message
vim.opt.shortmess:append { W = true, I = true, c = true }

-- Use system clipboard by default
vim.opt.clipboard:prepend("unnamed")

-- Default tab settings
vim.opt.tabstop = 2
vim.opt.softtabstop = -1
vim.opt.shiftwidth = 0
vim.opt.expandtab = true

-- Searching
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Case insensitive file name completion
vim.opt.wildignorecase = true

-- Scroll offset
vim.opt.scrolloff = 5

-- Use persistent undo
vim.opt.undofile = true

-- Line numbers
vim.opt.number = true
vim.opt.relativenumber = false

-- Color theme
vim.opt.termguicolors = true

-- Folding
vim.opt.foldenable = true
vim.opt.foldcolumn = "1"
vim.opt.foldlevel = 99
vim.opt.foldlevelstart = 99
vim.opt.fillchars = {
  fold = " ",
  foldopen = "",
  foldsep = " ",
  foldclose = "",
}
