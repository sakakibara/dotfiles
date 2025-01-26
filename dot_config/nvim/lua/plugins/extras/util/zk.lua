return {
  {
    "pkazmier/zk-nvim",
    branch = "snacks-picker",
    keys = {
      { "<Leader>zi", "<Cmd>ZkIndex<CR>", desc = "Index notes" },
      {
        "<Leader>zn",
        function()
          vim.ui.input({ prompt = "Title:" }, function(input)
            if input then
              require("zk").new({ title = input })
            end
          end)
        end,
        desc = "Create zk note",
      },
      { "<Leader>zn", ":'<,'>ZkNewFromTitleSelection<CR>", mode = { "v" }, desc = "New note from title selection" },
      { "<Leader>zN", ":'<,'>ZkNewFromContentSelection<CR>", mode = { "v" }, desc = "New note from content selection" },
      {
        "<Leader>zj",
        function()
          vim.ui.select({ "today", "yesterday", "tomorrow" }, {
            prompt = "Open journal:",
          }, function(choice)
            if choice then
              require("zk").new({
                date = choice == "today" and false or choice,
                dir = vim.env.ZK_NOTEBOOK_DIR .. Util.path.sep .. "journal",
                group = "journal",
              })
            end
          end)
        end,
        desc = "Create zk journal note",
      },
      { "<Leader>zc", "<Cmd>ZkCd<CR>", desc = "Cd into notebook root" },
      { "<Leader>zz", "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", desc = "Find notes" },
      { "<Leader>zf", "<Cmd>ZkBuffers<CR>", desc = "Find note buffers" },
      { "<Leader>zb", "<Cmd>ZkBacklinks<CR>", desc = "Backlinks" },
      { "<Leader>zl", "<Cmd>ZkLinks<CR>", desc = "Outbound links" },
      { "<Leader>zi", "<Cmd>ZkInsertLink<CR>", desc = "Insert link" },
      { "<Leader>zi", ":'<,'>ZkInsertLinkAtSelection<CR>", mode = { "v" }, desc = "Insert link at selection" },
      { "<Leader>zm", "<Cmd>ZkMatch<CR>", mode = { "x" }, desc = "Find note from selection" },
      { "<Leader>zt", "<Cmd>ZkTags<CR>", desc = "Find note by tags" },
      { "<Leader>z/", "<Cmd>ZkGrep<CR>", desc = "Grep notes" },
    },
    opts = {
      picker = Util.pick.picker.name == "snacks" and "snacks_picker" or "fzf" and "fzf_lua" or "telescope",

      lsp = {
        config = {
          cmd = { "zk", "lsp" },
          name = "zk",
        },

        auto_attach = {
          enabled = true,
          filetypes = { "markdown" },
        },
      },
    },
    config = function(_, opts)
      require("zk").setup(opts)

      local function snacks_grep_notes()
        Snacks.picker.grep({
          finder = "grep",
          format = "file",
          live = true,
          supports_live = true,
          dirs = { vim.env.ZK_NOTEBOOK_DIR },
        })
      end

      local function fzf_grep_notes()
        require("fzf-lua").live_grep({ prompt = "Grep Zk Notes ❯ ", cwd = vim.env.ZK_NOTEBOOK_DIR })
      end

      local function telescope_grep_notes()
        local collection = {}
        local list_opts = { select = { "title", "path", "absPath" } }
        require("zk.api").list(vim.env.ZK_NOTEBOOK_DIR, list_opts, function(_, notes)
          for _, note in ipairs(notes) do
            collection[note.absPath] = note.title or note.path
          end
        end)
        local options = vim.tbl_deep_extend("force", {
          prompt_title = "Notes",
          search_dirs = { vim.env.ZK_NOTEBOOK_DIR },
          disable_coordinates = true,
          path_display = function(_, path)
            return collection[path]
          end,
        }, opts or {})
        require("telescope.builtin").live_grep(options)
      end

      local grep_notes
      if Util.pick.picker.name == "snacks" then
        grep_notes = snacks_grep_notes
      elseif Util.pick.picker.name == "fzf" then
        grep_notes = fzf_grep_notes
      else
        grep_notes = telescope_grep_notes
      end

      vim.api.nvim_create_user_command("ZkGrep", grep_notes, {})
    end,
  },
}
