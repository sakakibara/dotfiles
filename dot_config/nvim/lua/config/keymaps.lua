local map = vim.keymap.set

map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
map({ "n", "x" }, "<Down>", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })
map({ "n", "x" }, "<Up>", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })

map("n", "<C-h>", "<C-w>h", { desc = "Go to left window", remap = true })
map("n", "<C-j>", "<C-w>j", { desc = "Go to lower window", remap = true })
map("n", "<C-k>", "<C-w>k", { desc = "Go to upper window", remap = true })
map("n", "<C-l>", "<C-w>l", { desc = "Go to right window", remap = true })

map("n", "<C-Up>", "<Cmd>resize +2<CR>", { desc = "Increase window height" })
map("n", "<C-Down>", "<Cmd>resize -2<CR>", { desc = "Decrease window height" })
map("n", "<C-Left>", "<Cmd>vertical resize -2<CR>", { desc = "Decrease window width" })
map("n", "<C-Right>", "<Cmd>vertical resize +2<CR>", { desc = "Increase window width" })

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

map("n", "[ ", "v:lua.Util.keymaps.put_empty_line(v:true)", { expr = true, desc = "Add empty line above" })
map("n", "] ", "v:lua.Util.keymaps.put_empty_line(v:false)", { expr = true, desc = "Add empty line below" })

map("n", "[q", vim.cmd.cprev, { desc = "Previous quickfix" })
map("n", "]q", vim.cmd.cnext, { desc = "Next quickfix" })

local diagnostic_goto = function(next, severity)
  local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    go({ severity = severity })
  end
end
map("n", "]d", diagnostic_goto(true), { desc = "Next diagnostic" })
map("n", "[d", diagnostic_goto(false), { desc = "Prev diagnostic" })
map("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next error" })
map("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev error" })
map("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next warning" })
map("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev warning" })

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

map("n", "<Leader>l", "<Cmd>Lazy<CR>", { desc = "Lazy" })

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

map("n", "<Leader>fT", function()
  Snacks.terminal()
end, { desc = "Terminal (cwd)" })
map("n", "<Leader>ft", function()
  Snacks.terminal(nil, { cwd = Util.root() })
end, { desc = "Terminal (root)" })
map("n", "<C-/>", function()
  Snacks.terminal(nil, { cwd = Util.root() })
end, { desc = "Terminal (root)" })
map("n", "<C-_>", function()
  Snacks.terminal(nil, { cwd = Util.root() })
end, { desc = "which_key_ignore" })
map("t", "<C-/>", "<Cmd>close<CR>", { desc = "Hide Terminal" })
map("t", "<C-_>", "<Cmd>close<CR>", { desc = "which_key_ignore" })

Util.format.snacks_toggle():map("<leader>uf")
Util.format.snacks_toggle(true):map("<leader>uF")
Snacks.toggle.option("spell", { name = "Spelling" }):map("<Leader>us")
Snacks.toggle.option("wrap", { name = "Wrap" }):map("<Leader>uw")
Snacks.toggle.option("relativenumber", { name = "Relative Number" }):map("<Leader>uL")
Snacks.toggle.diagnostics():map("<Leader>ud")
Snacks.toggle.line_number():map("<Leader>ul")
Snacks.toggle
  .option("conceallevel", { off = 0, on = vim.o.conceallevel > 0 and vim.o.conceallevel or 2, name = "Conceal Level" })
  :map("<Leader>uc")
Snacks.toggle
  .option("showtabline", { off = 0, on = vim.o.showtabline > 0 and vim.o.showtabline or 2, name = "Tabline" })
  :map("<Leader>uA")
Snacks.toggle.treesitter():map("<Leader>uT")
Snacks.toggle.option("background", { off = "light", on = "dark", name = "Dark Background" }):map("<Leader>ub")
Snacks.toggle.dim():map("<Leader>uD")
Snacks.toggle.animate():map("<Leader>ua")
Snacks.toggle.scroll():map("<Leader>uS")
Snacks.toggle.profiler():map("<Leader>dpp")
Snacks.toggle.profiler_highlights():map("<Leader>dph")

if vim.lsp.inlay_hint then
  Snacks.toggle.inlay_hints():map("<Leader>uh")
end

if vim.fn.executable("lazygit") == 1 then
  map("n", "<Leader>gg", function()
    Snacks.lazygit({ cwd = Util.root.git() })
  end, { desc = "Lazygit (root)" })
  map("n", "<Leader>gG", function()
    Snacks.lazygit()
  end, { desc = "Lazygit (cwd)" })
  map("n", "<Leader>gf", function()
    Snacks.lazygit.log_file()
  end, { desc = "Lazygit current file history" })
  map("n", "<Leader>gl", function()
    Snacks.lazygit.log({ cwd = Util.root.git() })
  end, { desc = "Lazygit log" })
  map("n", "<Leader>gL", function()
    Snacks.lazygit.log()
  end, { desc = "Lazygit log (cwd)" })
end

map("n", "<Leader>gb", function()
  Snacks.git.blame_line()
end, { desc = "Git blame line" })
map({ "n", "x" }, "<Leader>gB", function()
  Snacks.gitbrowse()
end, { desc = "Git browse (open)" })
map({ "n", "x" }, "<Leader>gY", function()
  Snacks.gitbrowse({
    open = function(url)
      vim.fn.setreg("+", url)
    end,
  })
end, { desc = "Git browse (copy)" })

if vim.fn.has("nvim-0.11") == 0 then
  map("s", "<Tab>", function()
    return vim.snippet.active({ direction = 1 }) and "<Cmd>lua vim.snippet.jump(1)<CR>" or "<Tab>"
  end, { expr = true, desc = "Jump next" })
  map({ "i", "s" }, "<S-Tab>", function()
    return vim.snippet.active({ direction = -1 }) and "<Cmd>lua vim.snippet.jump(-1)<CR>" or "<S-Tab>"
  end, { expr = true, desc = "Jump previous" })
end
