local map = vim.keymap.set
local ufile, ukeymaps = require("util.file"), require("util.keymaps")
ukeymaps.setup()

map("n", "<leader>w", "<C-w>")

map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

map("n", "<A-h>", "<C-w>h", { desc = "Go to the left window", remap = true })
map("n", "<A-j>", "<C-w>j", { desc = "Go to the lower window", remap = true })
map("n", "<A-k>", "<C-w>k", { desc = "Go to the upper window", remap = true })
map("n", "<A-l>", "<C-w>l", { desc = "Go to the right window", remap = true })
map("t", "<A-h>", "<cmd>wincmd h<cr>", { desc = "Go to the left window" })
map("t", "<A-j>", "<cmd>wincmd j<cr>", { desc = "Go to the lower window" })
map("t", "<A-k>", "<cmd>wincmd k<cr>", { desc = "Go to the upper window" })
map("t", "<A-l>", "<cmd>wincmd l<cr>", { desc = "Go to the right window" })

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

map("n", "<leader>ul", "<cmd>Lazy<cr>", { desc = "Lazy" })

map("n", "<leader>qq", "<cmd>qa<cr>", { desc = "Quit all" })

local conceallevel = vim.o.conceallevel > 0 and vim.o.conceallevel or 3
map("n", "<leader>to", function()
  ukeymaps.toggle("conceallevel", false, { 0, conceallevel })
end, { desc = "Toggle conceal" })

map("n", "<leader>td", function()
  ukeymaps.toggle_diagnostics()
end, { desc = "Toggle diagnostic" })

map("n", "<leader>tn", function()
  ukeymaps.toggle_number()
end, { desc = "Toggle line numbers" })

map("n", "<leader>tN", function()
  ukeymaps.toggle("number")
end, { desc = "Toggle 'number'" })

map("n", "<leader>tr", function()
  ukeymaps.toggle("relativenumber")
end, { desc = "Toggle 'relativenumber'" })

map("n", "<leader>ts", function()
  ukeymaps.toggle("spell")
end, { desc = "Toggle 'spell'" })

map("n", "<leader>tw", function()
  ukeymaps.toggle("wrap")
end, { desc = "Toggle 'wrap'" })

map("n", "<leader>tc", function()
  ukeymaps.toggle("cursorline")
end, { desc = "Toggle 'cursorline'" })

map("n", "<leader>tC", function()
  ukeymaps.toggle("cursorcolumn")
end, { desc = "Toggle 'cursorcolumn'" })

map("n", "<leader>ti", function()
  ukeymaps.toggle("ignorecase")
end, { desc = "Toggle 'ignorecase'" })

map("n", "<leader>tl", function()
  ukeymaps.toggle("list")
end, { desc = "Toggle 'list'" })

map("n", "<leader>tb", function()
  ukeymaps.toggle("background", false, { "dark", "light" })
end, { desc = "Toggle 'background'" })

map("n", "<leader>tf", function()
  require("util.format").toggle()
end, { desc = "Toggle auto format (global)" })

map("n", "<leader>tF", function()
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
