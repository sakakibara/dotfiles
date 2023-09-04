local map = vim.keymap.set

map("i", "jk", "<Esc>")
map("t", "jk", "<C-\\><C-n>")
map("n", "<leader>w", "<C-w>")

map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

map({ "n", "x", "o" }, "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search result" })
map({ "n", "x", "o" }, "N", "'nN'[v:searchforward]", { expr = true, desc = "Previous search result" })

map("n", "<leader>bn", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "<leader>bp", "<cmd>bNext<cr>", { desc = "Previous buffer" })
map("n", "<leader>bN", "<cmd>enew<cr>", { desc = "New empty buffer" })
map("n", "<leader>bs", "<cmd>w<cr>", { desc = "Save buffer" })

map("n", "<leader>xl", "<cmd>lopen<cr>", { desc = "Location list" })
map("n", "<leader>xq", "<cmd>copen<cr>", { desc = "Quickfix list" })

map("n", "<leader>[q", "<cmd>cprevious<cr>", { desc = "Previous quickfix" })
map("n", "<leader>]q", "<cmd>cnext<cr>", { desc = "Next quickfix" })

map("n", "<leader>ul", "<cmd>Lazy<cr>", { desc = "Lazy" })

map("n", "<leader>qq", "<cmd>qa<cr>", { desc = "Quit all" })

map("n", "<leader>td", function() require("util.keymaps").toggle_diagnostics() end, { desc = "Toggle diagnostic" })
map("n", "<leader>tn", function() require("util.keymaps").toggle_number() end, { desc = "Toggle line numbers" })
map("n", "<leader>tN", function() require("util.keymaps").toggle("number") end, { desc = "Toggle 'number'" })
map("n", "<leader>tr", function() require("util.keymaps").toggle("relativenumber") end, { desc = "Toggle 'relativenumber'" })
map("n", "<leader>ts", function() require("util.keymaps").toggle("spell") end, { desc = "Toggle 'spell'" })
map("n", "<leader>tw", function() require("util.keymaps").toggle("wrap") end, { desc = "Toggle 'wrap'" })
map("n", "<leader>tc", function() require("util.keymaps").toggle("cursorline") end, { desc = "Toggle 'cursorline'" })
map("n", "<leader>tC", function() require("util.keymaps").toggle("cursorcolumn") end, { desc = "Toggle 'cursorcolumn'" })
map("n", "<leader>ti", function() require("util.keymaps").toggle("ignorecase") end, { desc = "Toggle 'ignorecase'" })
map("n", "<leader>tl", function() require("util.keymaps").toggle("list") end, { desc = "Toggle 'list'" })
map("n", "<leader>tb", function() require("util.keymaps").toggle("background", false, {"dark", "light"}) end, { desc = "Toggle 'background'" })
