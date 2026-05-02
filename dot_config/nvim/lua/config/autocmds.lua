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

-- markdown: <CR> toggles GFM task-list checkboxes. Single keypress, scoped
-- to the buffer so it can't leak into quickfix/list buffers where <CR> is
-- "follow item". Normal mode toggles the current line; visual mode toggles
-- every line in the selection. The selection is re-entered after toggling
-- so repeated hits stay ergonomic — `<Cmd>` keymaps don't leave visual but
-- we need `'<`/`'>` marks (which only update on leaving visual) to know
-- the range, so we `<Esc>` out, toggle, then `gv` back in.
au("FileType", {
  group = grp,
  pattern = "markdown",
  callback = function(ev)
    vim.keymap.set("n", "<CR>", function()
      local row = vim.api.nvim_win_get_cursor(0)[1] - 1
      Lib.markdown.toggle_line(0, row)
    end, { buffer = ev.buf, desc = "Toggle checkbox" })

    vim.keymap.set("x", "<CR>", function()
      vim.cmd("normal! \27")
      local from = vim.api.nvim_buf_get_mark(0, "<")[1] - 1
      local to   = vim.api.nvim_buf_get_mark(0, ">")[1] - 1
      Lib.markdown.toggle_range(0, from, to)
      vim.cmd("normal! gv")
    end, { buffer = ev.buf, desc = "Toggle checkboxes in selection" })
  end,
})

-- WSL /mnt/* volumes: don't fix end-of-line (Windows line endings)
au({ "BufReadPre", "BufNewFile" }, {
  group = grp,
  pattern = "/mnt/*",
  callback = function() vim.opt_local.fixeol = false end,
})

-- Bigfile guardrails. snacks.bigfile registers a vim.filetype.add `[".*"]`
-- pattern at BufReadPre time, but in this loader's startup order it doesn't
-- always run before filetype detection on the first file (saw ft="" on a
-- 4MB *.panic dump with a 1.8MB single line — nvim froze rendering wrap
-- points across the line). We pre-empt at BufReadPre by stat'ing the file
-- on disk: if it's over the snacks default threshold we set ft=bigfile
-- ourselves and apply the same disables the plugin would have. The
-- treesitter FileType handler (lua/config/plugins/treesitter.lua:43)
-- already gates on this filetype, so no double-arming there.
local BIGFILE_BYTES = 1.5 * 1024 * 1024
au("BufReadPre", {
  group = grp,
  callback = function(args)
    if not args.file or args.file == "" then return end
    local size = vim.fn.getfsize(args.file)
    -- -2 means "too large for vim's number" — definitely bigfile.
    if not (size == -2 or size > BIGFILE_BYTES) then return end
    local buf = args.buf
    vim.bo[buf].syntax     = ""
    vim.bo[buf].synmaxcol  = 200
    vim.bo[buf].swapfile   = false
    vim.bo[buf].undolevels = -1
    vim.bo[buf].filetype   = "bigfile"
    vim.api.nvim_buf_call(buf, function()
      vim.opt_local.wrap         = false
      vim.opt_local.cursorline   = false
      vim.opt_local.foldmethod   = "manual"
      vim.opt_local.conceallevel = 0
      vim.opt_local.statuscolumn = ""
    end)
    if vim.fn.exists(":NoMatchParen") ~= 0 then vim.cmd("NoMatchParen") end
    vim.b[buf].minianimate_disable = true
  end,
})

-- highlight yank
au("TextYankPost", {
  group = grp,
  callback = function() vim.highlight.on_yank() end,
})

-- `q` closes transient/read-only popup buffers. Excludes `qf` so quickfix
-- can record macros. Plugins that manage their own dismiss UI (snacks_win,
-- noice notify, neotest, spectre, grug-far, gitsigns-blame, dbout) are
-- intentionally omitted — overriding q on their buffers can interfere with
-- their internal lifecycle.
au("FileType", {
  group = grp,
  pattern = { "PlenaryTestPopup", "checkhealth", "help", "lspinfo", "startuptime", "tsplayground" },
  callback = function(ev)
    vim.schedule(function()
      vim.keymap.set("n", "q", function()
        vim.cmd("close")
        pcall(vim.api.nvim_buf_delete, ev.buf, { force = true })
      end, { buffer = ev.buf, silent = true, desc = "Quit buffer" })
    end)
  end,
})

