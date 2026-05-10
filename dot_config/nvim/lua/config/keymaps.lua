-- lua/config/keymaps.lua
-- Leader must be set before any `<leader>x` keymap is created (incl. eager
-- plugin config()s during pack.setup). This file is required synchronously in
-- config/init.lua's stage 1 — costs ~1 ms warm but guarantees keymaps are
-- bound from t=0 instead of after the 30–100 ms UIEnter→VeryLazy window.
vim.g.mapleader      = " "
vim.g.maplocalleader = "\\"

local map = vim.keymap.set

-- Screenwise j/k: moves by visual lines when no count is given. Lives
-- as a toggle below (<Leader>uj) so it's easy to flip when working
-- with wrapped buffers vs. counted-jump muscle memory; the install
-- here is the default-ON state seen at startup, and the toggle's set()
-- below mirrors it.
local function set_screenwise_jk(state)
  if state then
    map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
    map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
  else
    pcall(vim.keymap.del, "n", "j"); pcall(vim.keymap.del, "x", "j")
    pcall(vim.keymap.del, "n", "k"); pcall(vim.keymap.del, "x", "k")
  end
end
set_screenwise_jk(vim.g.screenwise_jk ~= false)

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

-- leave terminal mode. <C-q> instead of <Esc>: <Esc><Esc> would impose a
-- timeoutlen wait on every plain <Esc> inside TUI apps (fzf, lazygit, htop).
map("t", "<C-q>", "<C-\\><C-n>", { desc = "Leave terminal mode" })

-- cmdline: fish-style accept-then-keep-typing. While the wildmenu/pum is
-- showing a completion candidate, <CR> accepts that candidate without
-- executing the command, so the user can continue typing more args. With
-- no popup active, <CR> executes as usual.
map("c", "<CR>", function()
  return vim.fn.wildmenumode() ~= 0 and "<C-y>" or "<CR>"
end, { expr = true })

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

-- empty line above/below (vim-unimpaired-style; preserves cursor, count
-- supported via v:count1).
map("n", "]<Space>", function() Lib.keymaps.put_empty_line(false) end, { desc = "Put empty line below" })
map("n", "[<Space>", function() Lib.keymaps.put_empty_line(true)  end, { desc = "Put empty line above" })

-- save / quit
map({ "i", "x", "n", "s" }, "<C-s>", "<Cmd>w<CR><Esc>", { desc = "Save" })
map("n", "<Leader>qq", "<Cmd>qa<CR>", { desc = "Quit all" })

-- buffer navigation
map("n", "[b", "<Cmd>bprevious<CR>", { desc = "Prev buffer" })
map("n", "]b", "<Cmd>bnext<CR>",     { desc = "Next buffer" })

-- quickfix / location-list navigation
map("n", "[q", vim.cmd.cprev, { desc = "Prev quickfix" })
map("n", "]q", vim.cmd.cnext, { desc = "Next quickfix" })
map("n", "[Q", vim.cmd.cfirst, { desc = "First quickfix" })
map("n", "]Q", vim.cmd.clast,  { desc = "Last quickfix" })
map("n", "[l", vim.cmd.lprev,  { desc = "Prev loclist" })
map("n", "]l", vim.cmd.lnext,  { desc = "Next loclist" })

-- search jump opens any fold the result is inside (zv) so we never land
-- on an invisible match that requires a manual `zo` to inspect.
map("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next search match" })
map("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Prev search match" })
map({ "x", "o" }, "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next search match" })
map({ "x", "o" }, "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev search match" })

-- undo break points: typing punctuation closes one undo block and opens
-- another, so a long insert can be undone in chunks rather than wiped
-- out wholesale.
map("i", ",", ",<C-g>u")
map("i", ".", ".<C-g>u")
map("i", ";", ";<C-g>u")

-- comment-aware open-line: gco / gcO open a new line below/above and
-- enter insert with the current filetype's comment leader pre-applied
-- (treesitter-commentstring populates &commentstring; gcc handles the
-- actual prefix work). Saves typing `// ` or `# ` by hand.
map("n", "gco", "o<Esc>Vcx<Esc><Cmd>normal gcc<CR>fxa<BS>", { desc = "Add comment below" })
map("n", "gcO", "O<Esc>Vcx<Esc><Cmd>normal gcc<CR>fxa<BS>", { desc = "Add comment above" })

-- tab management ( <Leader><Tab>… )
map("n", "<Leader><Tab><Tab>", "<Cmd>tabnew<CR>",      { desc = "New tab" })
map("n", "<Leader><Tab>]",     "<Cmd>tabnext<CR>",     { desc = "Next tab" })
map("n", "<Leader><Tab>[",     "<Cmd>tabprevious<CR>", { desc = "Prev tab" })
map("n", "<Leader><Tab>l",     "<Cmd>tablast<CR>",     { desc = "Last tab" })
map("n", "<Leader><Tab>f",     "<Cmd>tabfirst<CR>",    { desc = "First tab" })
map("n", "<Leader><Tab>d",     "<Cmd>tabclose<CR>",    { desc = "Close tab" })
map("n", "<Leader><Tab>o",     "<Cmd>tabonly<CR>",     { desc = "Close other tabs" })

-- single-keystroke window splits (mirror tmux's `prefix -` / `prefix |`)
map("n", "<Leader>-", "<C-w>s", { desc = "Split window below", remap = true })
map("n", "<Leader>|", "<C-w>v", { desc = "Split window right", remap = true })

-- drop unused nvim default tag-stack maps — :tag/:tnext/:tlast cmdline forms
-- still work if ever needed; ]t/[t go to other uses via core.pack override.
pcall(vim.keymap.del, "n", "]T")
pcall(vim.keymap.del, "n", "[T")

-- sibling-file navigation (same directory, wrap-around). [f/]f are taken by
-- treesitter-textobjects for function-start jumping; use uppercase F.
map("n", "[F", function() Lib.keymaps.cycle_sibling(-1) end, { desc = "Prev file in directory" })
map("n", "]F", function() Lib.keymaps.cycle_sibling( 1) end, { desc = "Next file in directory" })

-- winbar / statusline pickers — same menus the segments open on click
map("n", "<Leader>;",  function() Lib.winbar.pick_scope()       end, { desc = "Scope picker (sibling symbols)" })
map("n", "<Leader>.",  function() Lib.winbar.pick_path()        end, { desc = "Path picker (sibling files)" })
map("n", "<Leader>ut", function() Lib.keymaps.pick_filetype() end, { desc = "Set filetype" })

-- UI toggles ( <Leader>u… ). Each one wraps an option/feature in
-- Snacks.toggle so which-key renders the live "[on]/[off]" state in
-- the help popup — bare `<Cmd>... toggle<CR>` keymaps fire and forget,
-- which is why state stopped showing up after the rewrite. Deferred
-- via on_load because keymaps.lua runs before pack.setup loads snacks.
Lib.plugin.on_load("snacks.nvim", function()
  Snacks.toggle({
    name = "Autoformat (Global)",
    get  = function() return Lib.format.enabled() end,
    set  = function(state) Lib.format.enable(state) end,
  }):map("<Leader>uf")
  Snacks.toggle({
    name = "Autoformat (Buffer)",
    get  = function() return Lib.format.enabled(0) end,
    set  = function(state) Lib.format.enable(state, 0) end,
  }):map("<Leader>uF")
  Snacks.toggle({
    name = "Treesitter Context",
    get  = function()
      local ok, tsc = pcall(require, "treesitter-context")
      return ok and tsc.enabled() or false
    end,
    set  = function(state)
      local ok, tsc = pcall(require, "treesitter-context")
      if not ok then return end
      if state then tsc.enable() else tsc.disable() end
    end,
  }):map("<Leader>uT")
  Snacks.toggle.option("spell",          { name = "Spelling"        }):map("<Leader>us")
  Snacks.toggle.option("wrap",           { name = "Wrap"            }):map("<Leader>uw")
  Snacks.toggle.option("relativenumber", { name = "Relative Number" }):map("<Leader>uL")
  Snacks.toggle.option("conceallevel",
    { off = 0, on = vim.o.conceallevel > 0 and vim.o.conceallevel or 2, name = "Conceal" }
  ):map("<Leader>uC")
  Snacks.toggle.option("background",
    { off = "light", on = "dark", name = "Dark Background" }
  ):map("<Leader>ub")
  Snacks.toggle({
    name = "Screenwise j/k",
    get  = function() return vim.g.screenwise_jk ~= false end,
    set  = function(state) vim.g.screenwise_jk = state; set_screenwise_jk(state) end,
  }):map("<Leader>uj")
  Snacks.toggle.diagnostics():map("<Leader>ud")
  Snacks.toggle.line_number():map("<Leader>ul")
  Snacks.toggle.dim():map("<Leader>uD")
  Snacks.toggle.animate():map("<Leader>ua")
  Snacks.toggle.scroll():map("<Leader>uS")
  Snacks.toggle.zen():map("<Leader>uz")
  Snacks.toggle.zoom():map("<Leader>uZ")
  Snacks.toggle.profiler():map("<Leader>up")
  Snacks.toggle.profiler_highlights():map("<Leader>uP")
  if vim.lsp.inlay_hint then
    Snacks.toggle.inlay_hints():map("<Leader>uh")
  end
end)

map("n", "<Leader>ui", function() Lib.format.info() end, { desc = "Autoformat info" })
map("n", "<Leader>un", function()
  Snacks.notifier.hide()
  pcall(vim.cmd, "Noice dismiss")
end, { desc = "Dismiss all notifications" })

-- Pack subcommand picker. nvim's user-command rule forces `:Pack` to start
-- uppercase, so `:p<Tab>` autocompletes `packadd` rather than our `:Pack`.
-- This skips the cmdline entirely: pick a subcommand from a fuzzy list and
-- run it. The list is sourced from the same getcompletion path the cmdline
-- uses, so commands.lua remains the single source of truth.
map("n", "<Leader>up", function()
  local subs = vim.fn.getcompletion("Pack ", "cmdline")
  Snacks.picker.select(subs, { prompt = "Pack: " }, function(choice)
    if choice and choice ~= "" then vim.cmd("Pack " .. choice) end
  end)
end, { desc = "Pack subcommand" })

-- Pack: direct shortcuts for the no-arg daily-driver commands.
-- Arg-taking ones (reload/open/build/load/uninstall/rollback) stay on
-- the <Leader>up picker so completion handles the spec lookup.
map("n", "<Leader>Ps", "<Cmd>Pack status<CR>",   { desc = "Status" })
map("n", "<Leader>Pu", "<Cmd>Pack update<CR>",   { desc = "Update" })
map("n", "<Leader>PS", "<Cmd>Pack sync<CR>",     { desc = "Sync (update + clean)" })
map("n", "<Leader>Pi", "<Cmd>Pack install<CR>",  { desc = "Install missing" })
map("n", "<Leader>Pc", "<Cmd>Pack clean<CR>",    { desc = "Clean orphans" })
map("n", "<Leader>Pl", "<Cmd>Pack log<CR>",      { desc = "Log" })
map("n", "<Leader>Pp", "<Cmd>Pack profile<CR>",  { desc = "Profile" })

-- single-chord fast-access (skip a keystroke for the daily-driver ops).
-- Default scope is the project root (Lib.root: LSP / .git / lua); the
-- shifted variant forces the scope to cwd. Inside any picker prompt
-- `<A-c>` toggles between the two without closing.
map("n", "<Leader><Space>", function() Snacks.picker.smart({ cwd = Lib.root() }) end, { desc = "Smart find (root)" })
map("n", "<Leader>,",       function() Snacks.picker.buffers()                  end, { desc = "Buffers" })
map("n", "<Leader>/",       function() Snacks.picker.grep({ cwd = Lib.root() }) end, { desc = "Grep (root)" })
map("n", "<Leader>?",       function() Snacks.picker.grep()                     end, { desc = "Grep (cwd)" })
map("n", "<Leader>:",       function() Snacks.picker.command_history()          end, { desc = "Command history" })

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
