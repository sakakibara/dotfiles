-- lua/config/plugins/editor/nav.lua
-- File / URL navigation: directory editing, external links, search-replace,
-- dotfile helpers.

return {
  -- File explorer with inline-editable buffer: `-` opens parent dir
  {
    "stevearc/oil.nvim",
    lazy = false,  -- needed so `nvim <dir>` opens oil instead of netrw
    keys = {
      { "-", function() require("oil").open() end, desc = "Open parent directory" },
    },
    opts = {
      default_file_explorer = true,
      columns = {
        "icon",
        { "size",  highlight = "Number" },
        { "mtime", highlight = "Comment", format = "%Y-%m-%d %H:%M" },
      },
      view_options = { show_hidden = true },
      skip_confirm_for_simple_edits = true,
      keymaps = {
        ["g?"]    = "actions.show_help",
        ["<CR>"]  = "actions.select",
        ["<C-s>"] = { "actions.select", opts = { vertical = true } },
        ["<C-h>"] = false,  -- preserved for window nav
        ["<C-l>"] = false,
        ["<C-p>"] = "actions.preview",
        ["q"]     = "actions.close",
        ["<C-r>"] = "actions.refresh",
        ["-"]     = "actions.parent",
        ["_"]     = "actions.open_cwd",
        ["`"]     = "actions.cd",
        ["~"]     = { "actions.cd", opts = { scope = "tab" } },
        ["gs"]    = "actions.change_sort",
        ["gx"]    = "actions.open_external",
        ["g."]    = "actions.toggle_hidden",
        ["g\\"]   = "actions.toggle_trash",
      },
    },
  },

  -- LSP file operations: oil rename/move → workspace/didRename to server
  {
    "antosha417/nvim-lsp-file-operations",
    dependencies = { "plenary.nvim" },
    event = "LazyFile",
    opts = {},
  },

  -- Open URL / file path / issue reference under cursor
  {
    "chrishrb/gx.nvim",
    keys = { { "gx", "<Cmd>Browse<CR>", mode = { "n", "x" }, desc = "Open URL / path", override = true } },
    cmd  = { "Browse" },
    init = function() vim.g.netrw_nogx = 1 end,
    opts = { handler_options = { search_engine = "duckduckgo" } },
  },

  -- Search & replace across the workspace with live preview
  {
    "MagicDuck/grug-far.nvim",
    cmd  = "GrugFar",
    keys = {
      { "<Leader>sr", "<Cmd>GrugFar<CR>",             desc = "Search & replace" },
      { "<Leader>sR", "<Cmd>GrugFar<CR>", mode = "v", desc = "Search & replace (selection)" },
    },
    opts = { headerMaxWidth = 80 },
  },

  -- Chezmoi dotfile picker — source files only, skips template render detours
  {
    "xvzc/chezmoi.nvim",
    cmd  = { "ChezmoiEdit", "ChezmoiList" },
    keys = {
      { "<Leader>sz", function() vim.cmd("ChezmoiList") end, desc = "Chezmoi files" },
    },
    opts = { edit = { watch = false, force = false } },
  },

  -- Sudo read / write: :SudaRead, :SudaWrite
  { "lambdalisue/suda.vim", cmd = { "SudaRead", "SudaWrite" } },
}
