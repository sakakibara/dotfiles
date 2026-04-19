-- lua/config/options.lua
local opt = vim.opt

-- mise shims (unique to this user's tool-version setup)
vim.env.PATH = vim.fn.expand("~/.local/share/mise/shims") .. ":" .. vim.env.PATH

-- encodings with Japanese support
opt.fileencodings = { "utf-8", "iso-2022-jp", "cp932", "euc-jp", "default", "latin1" }

-- SSH-aware clipboard
if vim.env.SSH_TTY ~= nil then
  opt.clipboard = ""  -- no clipboard on remote sessions
else
  opt.clipboard = "unnamedplus"
end

-- folds
opt.foldenable = true
opt.foldlevel = 99
opt.foldlevelstart = 99
opt.foldcolumn = "3"
-- Nerd-font PUA glyphs need explicit cell width for fillchars (Neovim 0.12
-- rejects ambiguous-width chars with E1511). Tell it our fold chevrons + a
-- few common icon ranges render as 1-cell.
vim.fn.setcellwidths({
  { 0xE000, 0xF8FF, 1 },      -- BMP Private Use Area (nerd-font icons)
  { 0xF0000, 0xFFFFD, 1 },    -- Supplementary PUA-A
})

do
  local S = Lib.icons.status
  opt.fillchars = {
    foldopen  = S.FoldOpen,
    foldclose = S.FoldClose,
    fold      = S.FoldSeparator,
    foldsep   = S.FoldSeparator,
    diff      = "╱",
    eob       = " ",
  }
end

-- ui niceties (Neovim 0.11+)
opt.splitkeep = "screen"
opt.smoothscroll = true
opt.laststatus = 3
opt.showtabline = 1  -- only when >1 tab page

-- format
opt.formatexpr = "v:lua.Lib.format.formatexpr()"

-- root detection filters
vim.g.root_lsp_ignore = { "copilot" }

-- sensible defaults not already set by Neovim 0.12
opt.number = true
opt.relativenumber = true
opt.signcolumn = "yes"
opt.termguicolors = true
opt.updatetime = 200
opt.timeoutlen = 300
opt.undofile = true
opt.undolevels = 10000
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.smartindent = true
opt.ignorecase = true
opt.smartcase = true
opt.wrap = false
opt.scrolloff = 4
opt.sidescrolloff = 8
opt.winminwidth = 5
opt.shortmess:append("I")  -- no intro screen (Kuwasha message etc.)
opt.virtualedit = "block"
opt.conceallevel = 2
opt.confirm = true
