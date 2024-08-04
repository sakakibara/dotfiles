local ftadd = vim.filetype.add
local g, opt = vim.g, vim.opt

g.mapleader = " "
g.maplocalleader = "\\"
g.markdown_recommended_style = 0

opt.autowrite = true
opt.clipboard:prepend("unnamedplus")
opt.colorcolumn = "120"
opt.conceallevel = 3
opt.confirm = true
opt.cursorline = false
opt.expandtab = true
opt.fileencodings = { "ucs-bom", "utf-8", "iso-2022-jp", "cp932", "euc-jp", "latin1" }
opt.fileformats = { "unix", "dos", "mac" }
opt.fillchars = {
  fold = " ",
  foldclose = Util.config.icons.status.FoldClose,
  foldopen = Util.config.icons.status.FoldOpen,
  foldsep = Util.config.icons.status.FoldSeparator,
}
opt.foldcolumn = "1"
opt.foldenable = true
opt.foldlevel = 99
opt.foldlevelstart = 99
opt.formatexpr = "v:lua.require'util'.format.formatexpr()"
opt.formatoptions = "jcqlnt"
opt.grepformat = "%f:%l:%c:%m"
opt.grepprg = "rg --vimgrep"
opt.ignorecase = true
opt.laststatus = 3
opt.list = false
opt.mouse = "a"
opt.number = true
opt.pumblend = 10
opt.pumheight = 10
opt.relativenumber = false
opt.scrolloff = 4
opt.sessionoptions = { "buffers", "curdir", "tabpages", "winsize" }
opt.shiftround = true
opt.shiftwidth = 0
opt.shortmess:append({ W = true, I = true, c = true, C = true })
opt.showcmdloc = "statusline"
opt.showmode = false
opt.sidescrolloff = 8
opt.signcolumn = "yes"
opt.smartcase = true
opt.smartindent = true
opt.softtabstop = -1
opt.spelllang = { "en" }
opt.splitbelow = true
opt.splitkeep = "screen"
opt.splitright = true
opt.tabstop = 2
opt.termguicolors = true
opt.textwidth = 0
opt.timeoutlen = 300
opt.undofile = true
opt.undolevels = 10000
opt.updatetime = 4000
opt.wildignorecase = true
opt.wildmode = "longest:full,full"
opt.winminwidth = 5
opt.wrap = false

ftadd({ extension = { aspx = "html" } })
