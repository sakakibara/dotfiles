local map = vim.keymap.set
local ufile, ukeymaps, utoggle = require("util.file"), require("util.keymaps"), require("util.toggle")
ukeymaps.setup()

map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

map({ "n", "x", "o" }, "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map({ "n", "x", "o" }, "N", "'nN'[v:searchforward]", { expr = true, desc = "Previous search result" })

map("n", "<leader>bn", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "<leader>bp", "<cmd>bNext<cr>", { desc = "Previous buffer" })
map("n", "<leader>bN", "<cmd>enew<cr>", { desc = "New empty buffer" })
map("n", "<leader>bs", "<cmd>w<cr>", { desc = "Save buffer" })

map("n", "[Q", "<cmd>cfirst<cr>", { desc = "First quickfix" })
map("n", "]Q", "<cmd>clast<cr>", { desc = "Last quickfix" })
map("n", "[f", ufile.prev_file, { desc = "Previous file" })
map("n", "]f", ufile.next_file, { desc = "Next file" })
map("n", "[F", ufile.first_file, { desc = "First file" })
map("n", "]F", ufile.last_file, { desc = "Last file" })

map("n", "[ ", "v:lua.KeymapsUtil.put_empty_line(v:true)", { expr = true, desc = "Add empty line above" })
map("n", "] ", "v:lua.KeymapsUtil.put_empty_line(v:false)", { expr = true, desc = "Add empty line below" })

map("n", "<leader>fy", ukeymaps.yank_relative_path, { desc = "Yank relative path" })
map("n", "<leader>fY", ukeymaps.yank_full_path, { desc = "Yank full path" })

map({ "i", "n" }, "<esc>", "<cmd>nohlsearch<cr><esc>", { desc = "Escape and clear hlsearch" })
map(
  "n",
  "<leader>ur",
  "<cmd>nohlsearch<bar>diffupdate<bar>normal! <C-l><cr>",
  { desc = "Redraw / clear hlsearch / diff update" }
)

map("n", "<leader>ul", "<cmd>Lazy<cr>", { desc = "Lazy" })

map("n", "<leader>qq", "<cmd>qa<cr>", { desc = "Quit all" })

local conceallevel = vim.o.conceallevel > 0 and vim.o.conceallevel or 3
map("n", "<leader>oo", function()
  utoggle("conceallevel", false, { 0, conceallevel })
end, { desc = "Toggle conceal" })

if vim.lsp.buf.inlay_hint or vim.lsp.inlay_hint then
  map("n", "<leader>oh", function()
    utoggle.inlay_hints()
  end, { desc = "Toggle inlay hints" })
end

map("n", "<leader>oT", function()
  if vim.b.ts_highlight then
    vim.treesitter.stop()
  else
    vim.treesitter.start()
  end
end, { desc = "Toggle treesitter highlight" })

map("n", "<leader>od", function()
  utoggle.diagnostics()
end, { desc = "Toggle diagnostic" })

map("n", "<leader>on", function()
  utoggle.number()
end, { desc = "Toggle line numbers" })

map("n", "<leader>or", function()
  utoggle("relativenumber")
end, { desc = "Toggle 'relativenumber'" })

map("n", "<leader>os", function()
  utoggle("spell")
end, { desc = "Toggle 'spell'" })

map("n", "<leader>ow", function()
  utoggle("wrap")
end, { desc = "Toggle 'wrap'" })

map("n", "<leader>oc", function()
  utoggle("cursorline")
end, { desc = "Toggle 'cursorline'" })

map("n", "<leader>oC", function()
  utoggle("cursorcolumn")
end, { desc = "Toggle 'cursorcolumn'" })

map("n", "<leader>oi", function()
  utoggle("ignorecase")
end, { desc = "Toggle 'ignorecase'" })

map("n", "<leader>ol", function()
  utoggle("list")
end, { desc = "Toggle 'list'" })

map("n", "<leader>ob", function()
  utoggle("background", false, { "dark", "light" })
end, { desc = "Toggle 'background'" })

map("n", "<leader>of", function()
  require("util.format").toggle()
end, { desc = "Toggle auto format (global)" })

map("n", "<leader>oF", function()
  require("util.format").toggle(true)
end, { desc = "Toggle auto format (buffer)" })

map({ "n", "v" }, "<leader>cf", function()
  require("util.format").format({ force = true })
end, { desc = "Format" })

local function diagnostic_goto(next, severity)
  local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    go({ severity = severity })
  end
end

map("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line diagnostics" })
map("n", "]d", diagnostic_goto(true), { desc = "Next diagnostic" })
map("n", "[d", diagnostic_goto(false), { desc = "Prev diagnostic" })
map("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next error" })
map("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev error" })
map("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next warning" })
map("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev warning" })
