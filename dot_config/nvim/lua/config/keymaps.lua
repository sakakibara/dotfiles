local map = vim.keymap.set
map("i", "jk", "<Esc>")
map("n", "<Leader>w", "<C-w>")
map("n", "<Leader>bn", "<cmd>bnext<cr>", { desc = "Next buffer" })
map("n", "<Leader>bp", "<cmd>bNext<cr>", { desc = "Previous buffer" })
map("n", "<Leader>bN", "<cmd>enew<cr>", { desc = "New empty buffer" })
