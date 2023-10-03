local map = vim.keymap.set
local util_file = require("util.file")
local util_keymaps = require("util.keymaps")
util_keymaps.setup()

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
map("n", "[f", util_file.prev_file, { desc = "Previous file" })
map("n", "]f", util_file.next_file, { desc = "Next file" })
map("n", "[F", util_file.first_file, { desc = "First file" })
map("n", "]F", util_file.last_file, { desc = "Last file" })

map("n", "[ ", "v:lua.KeymapsUtil.put_empty_line(v:true)",  { expr = true, desc = "Add empty line above" })
map("n", "] ", "v:lua.KeymapsUtil.put_empty_line(v:false)", { expr = true, desc = "Add empty line below" })

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

map({ "n", "v" }, "<leader>cf", function()
  require("plugins.lsp.format").format({ force = true })
end, { desc = "Format" })
