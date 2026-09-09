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

-- folds. foldcolumn stays at its default 0: Lib.statuscolumn draws the
-- open/close indicators itself, and a foldcolumn above 0 leaks nested-level
-- digits into the gutter wherever statuscolumn is transiently cleared.
opt.foldlevel = 99
opt.foldlevelstart = 99
opt.foldmethod = "expr"
opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
opt.foldtext = "v:lua.Lib.fold.foldtext()"
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
-- UI layout: global statusline at the very bottom, no fixed cmdline row
-- (cmdline pops up via noice when needed), tabline only when >1 tab.
opt.laststatus  = 3
opt.cmdheight   = 0

-- Chrome format strings are set here (early) so the first painted frame
-- already has the bars. Lib.<bar>.setup() runs after plugins load and
-- registers the highlights and autocmds.
vim.o.statusline    = "%!v:lua.Lib.statusline.render()"
vim.o.winbar        = "%!v:lua.Lib.winbar.render()"
vim.o.tabline       = "%!v:lua.Lib.tabline.render()"
vim.o.statuscolumn  = "%!v:lua.Lib.statuscolumn.render()"

-- format
opt.formatexpr = "v:lua.Lib.format.formatexpr()"

-- root detection filters
vim.g.root_lsp_ignore = { "copilot" }

-- sensible defaults not already set by Neovim 0.12
opt.number = true
opt.relativenumber = true
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
