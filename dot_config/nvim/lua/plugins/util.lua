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
            nav_h = { "<C-h>", term_nav("h"), desc = "Go to Left Window", expr = true, mode = "t" },
            nav_j = { "<C-j>", term_nav("j"), desc = "Go to Lower Window", expr = true, mode = "t" },
            nav_k = { "<C-k>", term_nav("k"), desc = "Go to Upper Window", expr = true, mode = "t" },
            nav_l = { "<C-l>", term_nav("l"), desc = "Go to Right Window", expr = true, mode = "t" },
          },
        },
      },
      styles = {
        scratch = {
          zindex = 60,
        },
      },
    },
    keys = {
      {
        "<Leader>.",
        function()
          Snacks.scratch()
        end,
        desc = "Toggle Scratch Buffer",
      },
      {
        "<Leader>S",
        function()
          Snacks.scratch.select()
        end,
        desc = "Select Scratch Buffer",
      },
      {
        "<Leader>dps",
        function()
          Snacks.profiler.scratch()
        end,
        desc = "Profiler Scratch Buffer",
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
      { "<Leader>zi", "<Cmd>ZkIndex<CR>", desc = "Index Notes" },
      {
        "<Leader>zn",
        function()
          vim.ui.input({ prompt = "Title:" }, function(input)
            if input then
              require("zk").new({ title = input })
            end
          end)
        end,
        desc = "Create Zk Note",
      },
      { "<Leader>zn", ":'<,'>ZkNewFromTitleSelection<CR>", mode = { "v" }, desc = "New Note from Title Selection" },
      { "<Leader>zN", ":'<,'>ZkNewFromContentSelection<CR>", mode = { "v" }, desc = "New Note from Content Selection" },
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
        desc = "Create Zk Journal Note",
      },
      { "<Leader>zc", "<Cmd>ZkCd<CR>", desc = "Cd Into Notebook Root" },
      { "<Leader>zz", "<Cmd>ZkNotes { sort = { 'modified' } }<CR>", desc = "Find Notes" },
      { "<Leader>zf", "<Cmd>ZkBuffers<CR>", desc = "Find Note Buffers" },
      { "<Leader>zb", "<Cmd>ZkBacklinks<CR>", desc = "Backlinks" },
      { "<Leader>zl", "<Cmd>ZkLinks<CR>", desc = "Outbound Links" },
      { "<Leader>zi", "<Cmd>ZkInsertLink<CR>", desc = "Insert Link" },
      { "<Leader>zi", ":'<,'>ZkInsertLinkAtSelection<CR>", mode = { "v" }, desc = "Insert Link at Selection" },
      { "<Leader>zm", "<Cmd>ZkMatch<CR>", mode = { "x" }, desc = "Find Note from Selection" },
      { "<Leader>zt", "<Cmd>ZkTags<CR>", desc = "Find Note by Tags" },
      { "<Leader>z/", "<Cmd>ZkGrep<CR>", desc = "Grep Notes" },
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
