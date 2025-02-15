local map = vim.keymap.set

map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
map({ "n", "x" }, "<Down>", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })
map({ "n", "x" }, "<Up>", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })

map("n", "<C-h>", "<C-w>h", { desc = "Go to Left Window", remap = true })
map("n", "<C-j>", "<C-w>j", { desc = "Go to Lower Window", remap = true })
map("n", "<C-k>", "<C-w>k", { desc = "Go to Upper Window", remap = true })
map("n", "<C-l>", "<C-w>l", { desc = "Go to Right Window", remap = true })

map("n", "<C-Up>", "<Cmd>resize +2<CR>", { desc = "Increase Window Height" })
map("n", "<C-Down>", "<Cmd>resize -2<CR>", { desc = "Decrease Window Height" })
map("n", "<C-Left>", "<Cmd>vertical resize -2<CR>", { desc = "Decrease Window Width" })
map("n", "<C-Right>", "<Cmd>vertical resize +2<CR>", { desc = "Increase Window Width" })

map("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next Search Result" })
map({ "x", "o" }, "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
map("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Previous Search Result" })
map({ "x", "o" }, "N", "'nN'[v:searchforward]", { expr = true, desc = "Previous Search Result" })

map("i", ",", ",<C-g>u")
map("i", ".", ".<C-g>u")
map("i", ";", ";<C-g>u")

map("n", "<Leader>bn", "<Cmd>bnext<CR>", { desc = "Next Buffer" })
map("n", "<Leader>bp", "<Cmd>bNext<CR>", { desc = "Previous Buffer" })
map("n", "<Leader>bN", "<Cmd>enew<CR>", { desc = "New Empty Buffer" })
map("n", "<Leader>bs", "<Cmd>w<CR>", { desc = "Save Buffer" })
map("n", "<Leader>bd", function()
  Snacks.bufdelete()
end, { desc = "Delete Buffer" })
map("n", "<Leader>bo", function()
  Snacks.bufdelete.other()
end, { desc = "Delete Other Buffers" })
map("n", "<Leader>bD", "<Cmd>bd<CR>", { desc = "Delete Buffer and Window" })

map("n", "[ ", "v:lua.Util.keymaps.put_empty_line(v:true)", { expr = true, desc = "Add Empty Line Above" })
map("n", "] ", "v:lua.Util.keymaps.put_empty_line(v:false)", { expr = true, desc = "Add Empty Line Below" })

map("n", "[q", vim.cmd.cprev, { desc = "Previous Quickfix" })
map("n", "]q", vim.cmd.cnext, { desc = "Next Quickfix" })

local diagnostic_goto = function(next, severity)
  local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    go({ severity = severity })
  end
end
map("n", "]d", diagnostic_goto(true), { desc = "Next Diagnostic" })
map("n", "[d", diagnostic_goto(false), { desc = "Previoud Diagnostic" })
map("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next Error" })
map("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Previous Error" })
map("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next Warning" })
map("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Previous Warning" })

map("n", "<Leader>fy", Util.keymaps.yank_relative_path, { desc = "Yank Relative Path" })
map("n", "<Leader>fY", Util.keymaps.yank_full_path, { desc = "Yank Full Path" })

map({ "i", "n" }, "<Esc>", "<Cmd>nohlsearch<CR><Esc>", { desc = "Escape and Clear Hlsearch" })
map(
  "n",
  "<Leader>ur",
  "<Cmd>nohlsearch<bar>diffupdate<bar>normal! <C-l><CR>",
  { desc = "Redraw / Clear hlsearch / Diff Update" }
)

map("n", "gco", "o<Esc>Vcx<Esc><Cmd>normal gcc<CR>fxa<BS>", { desc = "Add Comment Below" })
map("n", "gcO", "O<Esc>Vcx<Esc><Cmd>normal gcc<CR>fxa<BS>", { desc = "Add Comment Above" })

map("n", "<Leader>l", "<Cmd>Lazy<CR>", { desc = "Lazy" })

map("n", "<Leader>qq", "<Cmd>qa<CR>", { desc = "Quit All" })

map({ "n", "v" }, "<Leader>cf", function()
  Util.format.run({ force = true })
end, { desc = "Format" })

map("n", "<Leader>cd", vim.diagnostic.open_float, { desc = "Line Diagnostics" })

map("n", "<Leader>wc", "<C-w>c", { desc = "Close a Window" })

map("n", "<Leader><Tab><Tab>", "<Cmd>tabnew<CR>", { desc = "New Tab" })
map("n", "<Leader><Tab>[", "<Cmd>tabprevious<CR>", { desc = "Previous Tab" })
map("n", "<Leader><Tab>]", "<Cmd>tabnext<CR>", { desc = "Next Tab" })
map("n", "<Leader><Tab>c", "<Cmd>tabclose<CR>", { desc = "Close Tab" })
map("n", "<Leader><Tab>f", "<Cmd>tabfirst<CR>", { desc = "First Tab" })
map("n", "<Leader><Tab>l", "<Cmd>tablast<CR>", { desc = "Last Tab" })
map("n", "<Leader><Tab>o", "<Cmd>tabonly<CR>", { desc = "Close Other Tabs" })
map("n", "<Leader><Tab>q", "<Cmd>tabclose<CR>", { desc = "Close Tab" })

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
  end, { desc = "Lazygit Current File History" })
  map("n", "<Leader>gl", function()
    Snacks.lazygit.log({ cwd = Util.root.git() })
  end, { desc = "Lazygit Log" })
  map("n", "<Leader>gL", function()
    Snacks.lazygit.log()
  end, { desc = "Lazygit Log (cwd)" })
end

map("n", "<Leader>gb", function()
  Snacks.git.blame_line()
end, { desc = "Git Blame Line" })
map({ "n", "x" }, "<Leader>gB", function()
  Snacks.gitbrowse()
end, { desc = "Git Browse (open)" })
map({ "n", "x" }, "<Leader>gY", function()
  Snacks.gitbrowse({
    open = function(url)
      vim.fn.setreg("+", url)
    end,
  })
end, { desc = "Git Browse (copy)" })

if vim.fn.has("nvim-0.11") == 0 then
  map("s", "<Tab>", function()
    return vim.snippet.active({ direction = 1 }) and "<Cmd>lua vim.snippet.jump(1)<CR>" or "<Tab>"
  end, { expr = true, desc = "Jump Next" })
  map({ "i", "s" }, "<S-Tab>", function()
    return vim.snippet.active({ direction = -1 }) and "<Cmd>lua vim.snippet.jump(-1)<CR>" or "<S-Tab>"
  end, { expr = true, desc = "Jump Previous" })
end
