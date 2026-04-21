-- lua/config/keymaps.lua
local map = vim.keymap.set

-- smooth wrapped-line motion
map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- window navigation
map("n", "<C-h>", "<C-w>h", { desc = "Left window" })
map("n", "<C-j>", "<C-w>j", { desc = "Down window" })
map("n", "<C-k>", "<C-w>k", { desc = "Up window" })
map("n", "<C-l>", "<C-w>l", { desc = "Right window" })

-- window resize
map("n", "<C-Up>",    "<cmd>resize +2<cr>",          { desc = "Resize up" })
map("n", "<C-Down>",  "<cmd>resize -2<cr>",          { desc = "Resize down" })
map("n", "<C-Left>",  "<cmd>vertical resize -2<cr>", { desc = "Resize left" })
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Resize right" })

-- clear search highlight
map({ "i", "n" }, "<esc>", "<cmd>noh<cr><esc>", { desc = "Escape + clear hlsearch" })

-- better paste/indent
map("v", "<", "<gv")
map("v", ">", ">gv")

-- diagnostics
local diag = function(next, severity)
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function() vim.diagnostic.jump({ count = next and 1 or -1, severity = severity, float = true }) end
end
map("n", "]d", diag(true),           { desc = "Next diagnostic" })
map("n", "[d", diag(false),          { desc = "Prev diagnostic" })
map("n", "]e", diag(true,  "ERROR"), { desc = "Next error" })
map("n", "[e", diag(false, "ERROR"), { desc = "Prev error" })
map("n", "]w", diag(true,  "WARN"),  { desc = "Next warning" })
map("n", "[w", diag(false, "WARN"),  { desc = "Prev warning" })

-- path yanks (Lib.keymaps helpers)
map("n", "<leader>fy", function() Lib.keymaps.yank_relative_path() end, { desc = "Yank relative path" })
map("n", "<leader>fY", function() Lib.keymaps.yank_full_path()     end, { desc = "Yank absolute path" })

-- empty line above/below (preserves cursor, count supported via v:count1)
map("n", "<leader>o", function() Lib.keymaps.put_empty_line(false) end, { desc = "Put empty line below" })
map("n", "<leader>O", function() Lib.keymaps.put_empty_line(true)  end, { desc = "Put empty line above" })

-- save / quit
map({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save" })
map("n", "<leader>qq", "<cmd>qa<cr>", { desc = "Quit all" })

-- buffer navigation
map("n", "[b", "<cmd>bprevious<cr>", { desc = "Prev buffer" })
map("n", "]b", "<cmd>bnext<cr>",     { desc = "Next buffer" })

-- winbar pickers — same menus the breadcrumb segments open on click
map("n", "<leader>;", function() Lib.winbar.pick_scope() end, { desc = "Scope picker (sibling symbols)" })
map("n", "<leader>.", function() Lib.winbar.pick_path()  end, { desc = "Path picker (sibling files)" })

-- autoformat toggles (UI toggles convention: <leader>u…)
map("n", "<leader>uf", function() Lib.format.toggle()     end, { desc = "Toggle autoformat (global)" })
map("n", "<leader>uF", function() Lib.format.toggle(true) end, { desc = "Toggle autoformat (buffer)" })
map("n", "<leader>ui", function() Lib.format.info()       end, { desc = "Autoformat info" })

-- single-chord fast-access (skip a keystroke for the daily-driver ops)
map("n", "<leader><space>", function() Snacks.picker.smart()            end, { desc = "Smart find (files)" })
map("n", "<leader>,",       function() Snacks.picker.buffers()          end, { desc = "Buffers" })
map("n", "<leader>/",       function() Snacks.picker.grep()             end, { desc = "Grep" })
map("n", "<leader>:",       function() Snacks.picker.command_history()  end, { desc = "Command history" })

-- windows (<C-w>… natives still work; these are leader aliases for which-key)
map("n", "<leader>ws", "<C-w>s",           { desc = "Split window below" })
map("n", "<leader>wv", "<C-w>v",           { desc = "Split window right" })
map("n", "<leader>wc", "<cmd>close<cr>",   { desc = "Close window" })
map("n", "<leader>wo", "<cmd>only<cr>",    { desc = "Close other windows" })
map("n", "<leader>w=", "<C-w>=",           { desc = "Equalize windows" })

-- file explorer (oil is also on `-` for parent-dir)
map("n", "<leader>e", "<cmd>Oil<cr>", { desc = "Oil (parent dir)" })
