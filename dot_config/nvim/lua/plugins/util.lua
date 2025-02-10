local function term_nav(dir)
  return function(self)
    return self:is_floating() and "<C-" .. dir .. ">" or vim.schedule(function()
      vim.cmd.wincmd(dir)
    end)
  end
end

return {
  {
    "snacks.nvim",
    opts = {
      bigfile = { enabled = true },
      quickfile = { enabled = true },
      terminal = {
        win = {
          keys = {
            nav_h = { "<C-h>", term_nav("h"), desc = "Go to left window", expr = true, mode = "t" },
            nav_j = { "<C-j>", term_nav("j"), desc = "Go to lower window", expr = true, mode = "t" },
            nav_k = { "<C-k>", term_nav("k"), desc = "Go to upper window", expr = true, mode = "t" },
            nav_l = { "<C-l>", term_nav("l"), desc = "Go to right window", expr = true, mode = "t" },
          },
        },
      },
    },
    keys = {
      {
        "<Leader>.",
        function()
          Snacks.scratch()
        end,
        desc = "Toggle scratch buffer",
      },
      {
        "<Leader>S",
        function()
          Snacks.scratch.select()
        end,
        desc = "Select scratch buffer",
      },
      {
        "<Leader>dps",
        function()
          Snacks.profiler.scratch()
        end,
        desc = "Profiler scratch buffer",
      },
    },
  },

  {
    "alker0/chezmoi.vim",
    init = function()
      vim.g["chezmoi#use_tmp_buffer"] = 1
      vim.g["chezmoi#source_dir_path"] = os.getenv("HOME") .. "/.local/share/chezmoi"
    end,
  },

  {
    "xvzc/chezmoi.nvim",
    keys = {
      {
        "<Leader>sz",
        function()
          local results = require("chezmoi.commands").list({
            args = {
              "--path-style",
              "absolute",
              "--include",
              "files",
              "--exclude",
              "externals",
            },
          })
          local items = {}

          for _, czFile in ipairs(results) do
            table.insert(items, {
              text = czFile,
              file = czFile,
            })
          end

          local opts = {
            items = items,
            confirm = function(picker, item)
              picker:close()
              require("chezmoi.commands").edit({
                targets = { item.text },
                args = { "--watch" },
              })
            end,
          }
          Snacks.picker.pick(opts)
        end,
        desc = "Chezmoi",
      },
    },
    opts = {
      edit = {
        watch = false,
        force = false,
      },
      notification = {
        on_open = true,
        on_apply = true,
        on_watch = false,
      },
      telescope = {
        select = { "<CR>" },
      },
    },
    init = function()
      vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        pattern = { os.getenv("HOME") .. "/.local/share/chezmoi/*" },
        callback = function()
          vim.schedule(require("chezmoi.commands.__edit").watch)
        end,
      })
    end,
  },

  {
    "zk-org/zk-nvim",
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

      local function grep_notes()
        Snacks.picker.grep({
          finder = "grep",
          format = "file",
          live = true,
          supports_live = true,
          dirs = { vim.env.ZK_NOTEBOOK_DIR },
        })
      end

      vim.api.nvim_create_user_command("ZkGrep", grep_notes, {})
    end,
  },
}
