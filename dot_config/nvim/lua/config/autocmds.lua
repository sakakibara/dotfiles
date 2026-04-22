-- lua/config/autocmds.lua
local au = vim.api.nvim_create_autocmd
local grp = vim.api.nvim_create_augroup("Lib", { clear = true })

-- Swap handling: rely on Neovim 0.12's default nvim.swapfile augroup
-- (runtime/lua/vim/_core/defaults.lua). It picks "e" when a live nvim
-- owns the swap and prompts otherwise — same as LazyVim's behavior.

-- auto-create parent directories on save
au("BufWritePre", {
  group = grp,
  callback = function(event)
    if event.match:match("^%w%w+:[\\/][\\/]") then return end  -- skip scp://, oil:// etc
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
  end,
})

-- visual-mode relative numbers: toggle off in visual for clearer selection
au("ModeChanged", {
  group = grp,
  pattern = { "*:[vV\x16]*", "[vV\x16]*:*" },
  callback = function()
    -- Skip floats (menus, pickers, etc.) — they manage their own UI and
    -- have no business showing line numbers regardless of mode.
    if vim.api.nvim_win_get_config(0).relative ~= "" then return end
    local is_visual = vim.fn.mode():match("[vV\x16]")
    vim.opt_local.relativenumber = not is_visual
  end,
})

-- markdown / json: show formatting
au("FileType", {
  group = grp,
  pattern = { "markdown", "json", "jsonc" },
  callback = function() vim.opt_local.conceallevel = 0 end,
})

-- markdown: visible 80-col guide
au("FileType", {
  group = grp,
  pattern = "markdown",
  callback = function() vim.opt_local.colorcolumn = "80" end,
})

-- WSL /mnt/* volumes: don't fix end-of-line (Windows line endings)
au({ "BufReadPre", "BufNewFile" }, {
  group = grp,
  pattern = "/mnt/*",
  callback = function() vim.opt_local.fixeol = false end,
})

-- highlight yank
au("TextYankPost", {
  group = grp,
  callback = function() vim.highlight.on_yank() end,
})
