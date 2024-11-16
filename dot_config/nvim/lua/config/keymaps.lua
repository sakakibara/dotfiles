local map = vim.keymap.set

map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

map("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next search result" })
map({ "x", "o" }, "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Previous search result" })
map({ "x", "o" }, "N", "'nN'[v:searchforward]", { expr = true, desc = "Previous search result" })

map("i", ",", ",<C-g>u")
map("i", ".", ".<C-g>u")
map("i", ";", ";<C-g>u")

map("n", "<Leader>bn", "<Cmd>bnext<CR>", { desc = "Next buffer" })
map("n", "<Leader>bp", "<Cmd>bNext<CR>", { desc = "Previous buffer" })
map("n", "<Leader>bN", "<Cmd>enew<CR>", { desc = "New empty buffer" })
map("n", "<Leader>bs", "<Cmd>w<CR>", { desc = "Save buffer" })
map("n", "<Leader>bd", function()
  Snacks.bufdelete()
end, { desc = "Delete buffer" })
map("n", "<Leader>bo", function()
  Snacks.bufdelete.other()
end, { desc = "Delete other buffers" })
map("n", "<Leader>bD", "<Cmd>bd<CR>", { desc = "Delete buffer and window" })

map("n", "<Leader>fy", Util.keymaps.yank_relative_path, { desc = "Yank relative path" })
map("n", "<Leader>fY", Util.keymaps.yank_full_path, { desc = "Yank full path" })

map({ "i", "n" }, "<Esc>", "<Cmd>nohlsearch<CR><Esc>", { desc = "Escape and clear hlsearch" })
map(
  "n",
  "<Leader>ur",
  "<Cmd>nohlsearch<bar>diffupdate<bar>normal! <C-l><CR>",
  { desc = "Redraw / clear hlsearch / diff update" }
)

map("n", "gco", "o<Esc>Vcx<Esc><Cmd>normal gcc<CR>fxa<BS>", { desc = "Add comment below" })
map("n", "gcO", "O<Esc>Vcx<Esc><Cmd>normal gcc<CR>fxa<BS>", { desc = "Add comment above" })

map("n", "<Leader>ul", "<Cmd>Lazy<CR>", { desc = "Lazy" })

map("n", "<Leader>qq", "<Cmd>qa<CR>", { desc = "Quit all" })

map({ "n", "v" }, "<Leader>cf", function()
  Util.format.run({ force = true })
end, { desc = "Format" })

map("n", "<Leader>cd", vim.diagnostic.open_float, { desc = "Line diagnostics" })

map("n", "<Leader>wc", "<C-w>c", { desc = "Close a window" })

map("n", "<Leader><Tab><Tab>", "<Cmd>tabnew<CR>", { desc = "New tab" })
map("n", "<Leader><Tab>[", "<Cmd>tabprevious<CR>", { desc = "Previous tab" })
map("n", "<Leader><Tab>]", "<Cmd>tabnext<CR>", { desc = "Next tab" })
map("n", "<Leader><Tab>c", "<Cmd>tabclose<CR>", { desc = "Close tab" })
map("n", "<Leader><Tab>f", "<Cmd>tabfirst<CR>", { desc = "First tab" })
map("n", "<Leader><Tab>l", "<Cmd>tablast<CR>", { desc = "Last tab" })
map("n", "<Leader><Tab>o", "<Cmd>tabonly<CR>", { desc = "Close other tabs" })
map("n", "<Leader><Tab>q", "<Cmd>tabclose<CR>", { desc = "Close tab" })

if vim.fn.has("nvim-0.11") == 0 then
  map("s", "<Tab>", function()
    return vim.snippet.active({ direction = 1 }) and "<Cmd>lua vim.snippet.jump(1)<CR>" or "<Tab>"
  end, { expr = true, desc = "Jump next" })
  map({ "i", "s" }, "<S-Tab>", function()
    return vim.snippet.active({ direction = -1 }) and "<Cmd>lua vim.snippet.jump(-1)<CR>" or "<S-Tab>"
  end, { expr = true, desc = "Jump previous" })
end
