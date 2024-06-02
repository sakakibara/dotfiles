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

map("n", "[Q", "<Cmd>cfirst<CR>", { desc = "First quickfix" })
map("n", "]Q", "<Cmd>clast<CR>", { desc = "Last quickfix" })
map("n", "[f", Util.file.prev_file, { desc = "Previous file" })
map("n", "]f", Util.file.next_file, { desc = "Next file" })
map("n", "[F", Util.file.first_file, { desc = "First file" })
map("n", "]F", Util.file.last_file, { desc = "Last file" })

map("n", "[ ", "v:lua.Util.keymaps.put_empty_line(v:true)", { expr = true, desc = "Add empty line above" })
map("n", "] ", "v:lua.Util.keymaps.put_empty_line(v:false)", { expr = true, desc = "Add empty line below" })

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

local conceallevel = vim.o.conceallevel > 0 and vim.o.conceallevel or 3
map("n", "<Leader>oo", function()
  Util.toggle("conceallevel", false, { 0, conceallevel })
end, { desc = "Toggle conceal" })

if vim.lsp.buf.inlay_hint or vim.lsp.inlay_hint then
  map("n", "<Leader>oh", function()
    Util.toggle.inlay_hints()
  end, { desc = "Toggle inlay hints" })
end

map("n", "<Leader>oT", function()
  if vim.b.ts_highlight then
    vim.treesitter.stop()
  else
    vim.treesitter.start()
  end
end, { desc = "Toggle treesitter highlight" })

map("n", "<Leader>od", function()
  Util.toggle.diagnostics()
end, { desc = "Toggle diagnostic" })

map("n", "<Leader>on", function()
  Util.toggle.number()
end, { desc = "Toggle line numbers" })

map("n", "<Leader>or", function()
  Util.toggle("relativenumber")
end, { desc = "Toggle 'relativenumber'" })

map("n", "<Leader>os", function()
  Util.toggle("spell")
end, { desc = "Toggle 'spell'" })

map("n", "<Leader>ow", function()
  Util.toggle("wrap")
end, { desc = "Toggle 'wrap'" })

map("n", "<Leader>oc", function()
  Util.toggle("cursorline")
end, { desc = "Toggle 'cursorline'" })

map("n", "<Leader>oC", function()
  Util.toggle("cursorcolumn")
end, { desc = "Toggle 'cursorcolumn'" })

map("n", "<Leader>oi", function()
  Util.toggle("ignorecase")
end, { desc = "Toggle 'ignorecase'" })

map("n", "<Leader>ol", function()
  Util.toggle("list")
end, { desc = "Toggle 'list'" })

map("n", "<Leader>ob", function()
  Util.toggle("background", false, { "dark", "light" })
end, { desc = "Toggle 'background'" })

map("n", "<Leader>of", function()
  Util.format.toggle()
end, { desc = "Toggle auto format (global)" })

map("n", "<Leader>oF", function()
  Util.format.toggle(true)
end, { desc = "Toggle auto format (buffer)" })

map({ "n", "v" }, "<Leader>cf", function()
  Util.format.run({ force = true })
end, { desc = "Format" })

local function diagnostic_goto(next, severity)
  local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    go({ severity = severity })
  end
end

map("n", "<Leader>cd", vim.diagnostic.open_float, { desc = "Line diagnostics" })
map("n", "]d", diagnostic_goto(true), { desc = "Next diagnostic" })
map("n", "[d", diagnostic_goto(false), { desc = "Prev diagnostic" })
map("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next error" })
map("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev error" })
map("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next warning" })
map("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev warning" })
