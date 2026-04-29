-- lua/config/keymaps.lua
-- Leader must be set before any `<leader>x` keymap is created (incl. eager
-- plugin config()s during pack.setup). This file is required synchronously in
-- config/init.lua's stage 1 — costs ~1 ms warm but guarantees keymaps are
-- bound from t=0 instead of after the 30–100 ms UIEnter→VeryLazy window.
vim.g.mapleader      = " "
vim.g.maplocalleader = "\\"

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
map("n", "<C-Up>",    "<Cmd>resize +2<CR>",          { desc = "Resize up" })
map("n", "<C-Down>",  "<Cmd>resize -2<CR>",          { desc = "Resize down" })
map("n", "<C-Left>",  "<Cmd>vertical resize -2<CR>", { desc = "Resize left" })
map("n", "<C-Right>", "<Cmd>vertical resize +2<CR>", { desc = "Resize right" })

-- clear search highlight
map({ "i", "n" }, "<Esc>", "<Cmd>noh<CR><Esc>", { desc = "Escape + clear hlsearch" })

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
map("n", "<Leader>fy", function() Lib.keymaps.yank_relative_path() end, { desc = "Yank relative path" })
map("n", "<Leader>fY", function() Lib.keymaps.yank_full_path()     end, { desc = "Yank absolute path" })

-- empty line above/below (preserves cursor, count supported via v:count1)
map("n", "<Leader>o", function() Lib.keymaps.put_empty_line(false) end, { desc = "Put empty line below" })
map("n", "<Leader>O", function() Lib.keymaps.put_empty_line(true)  end, { desc = "Put empty line above" })

-- save / quit
map({ "i", "x", "n", "s" }, "<C-s>", "<Cmd>w<CR><Esc>", { desc = "Save" })
map("n", "<Leader>qq", "<Cmd>qa<CR>", { desc = "Quit all" })

-- buffer navigation
map("n", "[b", "<Cmd>bprevious<CR>", { desc = "Prev buffer" })
map("n", "]b", "<Cmd>bnext<CR>",     { desc = "Next buffer" })

-- winbar / statusline pickers — same menus the segments open on click
map("n", "<Leader>;",  function() Lib.winbar.pick_scope()       end, { desc = "Scope picker (sibling symbols)" })
map("n", "<Leader>.",  function() Lib.winbar.pick_path()        end, { desc = "Path picker (sibling files)" })
map("n", "<Leader>ut", function() Lib.keymaps.pick_filetype() end, { desc = "Set filetype" })

-- autoformat toggles (UI toggles convention: <Leader>u…)
map("n", "<Leader>uf", function() Lib.format.toggle()     end, { desc = "Toggle autoformat (global)" })
map("n", "<Leader>uF", function() Lib.format.toggle(true) end, { desc = "Toggle autoformat (buffer)" })
map("n", "<Leader>ui", function() Lib.format.info()       end, { desc = "Autoformat info" })
map("n", "<Leader>uT", "<Cmd>TSContext toggle<CR>", { desc = "Toggle treesitter context" })
map("n", "<Leader>un", function()
  Snacks.notifier.hide()
  pcall(vim.cmd, "Noice dismiss")
end, { desc = "Dismiss all notifications" })

-- single-chord fast-access (skip a keystroke for the daily-driver ops)
map("n", "<Leader><Space>", function() Snacks.picker.smart()            end, { desc = "Smart find (files)" })
map("n", "<Leader>,",       function() Snacks.picker.buffers()          end, { desc = "Buffers" })
map("n", "<Leader>/",       function() Snacks.picker.grep()             end, { desc = "Grep" })
map("n", "<Leader>:",       function() Snacks.picker.command_history()  end, { desc = "Command history" })

-- windows (<C-w>… natives still work; these are leader aliases for which-key)
map("n", "<Leader>ws", "<C-w>s",           { desc = "Split window below" })
map("n", "<Leader>wv", "<C-w>v",           { desc = "Split window right" })
map("n", "<Leader>wc", "<Cmd>close<CR>",   { desc = "Close window" })
map("n", "<Leader>wo", "<Cmd>only<CR>",    { desc = "Close other windows" })
map("n", "<Leader>w=", "<C-w>=",           { desc = "Equalize windows" })

-- file explorer (oil is also on `-` for parent-dir)
map("n", "<Leader>e", "<Cmd>Oil<CR>", { desc = "Oil (parent dir)" })

-- color picker (Lib.colors)
vim.keymap.set("n", "<Leader>uc", function() vim.cmd("ColorPick") end, { desc = "Color picker" })
