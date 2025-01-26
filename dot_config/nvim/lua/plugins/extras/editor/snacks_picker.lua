local picker = {
  name = "snacks",
  commands = {
    files = "files",
    live_grep = "grep",
    oldfiles = "recent",
  },

  open = function(source, opts)
    return Snacks.picker.pick(source, opts)
  end,
}
if not Util.pick.register(picker) then
  return {}
end

return {
  {
    "folke/snacks.nvim",
    opts = {
      picker = {
        win = {
          input = {
            keys = {
              ["<A-c>"] = {
                "toggle_cwd",
                mode = { "n", "i" },
              },
            },
          },
        },
        actions = {
          toggle_cwd = function(p)
            local root = Util.root({ buf = p.input.filter.current_buf, normalize = true })
            local cwd = vim.fs.normalize((vim.uv or vim.loop).cwd() or ".")
            local current = p:cwd()
            p:set_cwd(current == root and cwd or root)
            p:find()
          end,
        },
      },
    },
    keys = {
      {
        "<Leader>,",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Buffers",
      },
      { "<Leader>/", Util.pick("grep"), desc = "Grep (root)" },
      {
        "<Leader>:",
        function()
          Snacks.picker.command_history()
        end,
        desc = "Command history",
      },
      { "<Leader><Space>", Util.pick("files"), desc = "Find files (root)" },
      {
        "<Leader>fb",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Buffers",
      },
      {
        "<Leader>fB",
        function()
          Snacks.picker.buffers({ hidden = true, nofile = true })
        end,
        desc = "Buffers (all)",
      },
      { "<Leader>fc", Util.pick.config_files(), desc = "Find config file" },
      { "<Leader>ff", Util.pick("files"), desc = "Find files (root)" },
      { "<Leader>fF", Util.pick("files", { root = false }), desc = "Find files (cwd)" },
      {
        "<Leader>fg",
        function()
          Snacks.picker.git_files()
        end,
        desc = "Find files (git-files)",
      },
      { "<Leader>fr", Util.pick("oldfiles"), desc = "Recent" },
      { "<Leader>fR", Util.pick("oldfiles", { filter = { cwd = true } }), desc = "Recent (cwd)" },
      {
        "<Leader>fp",
        function()
          Snacks.picker.projects()
        end,
        desc = "Projects",
      },
      {
        "<Leader>gc",
        function()
          Snacks.picker.git_log()
        end,
        desc = "Git log",
      },
      {
        "<Leader>gd",
        function()
          Snacks.picker.git_diff()
        end,
        desc = "Git diff (hunks)",
      },
      {
        "<Leader>gs",
        function()
          Snacks.picker.git_status()
        end,
        desc = "Git status",
      },
      {
        "<Leader>sb",
        function()
          Snacks.picker.lines()
        end,
        desc = "Buffer lines",
      },
      {
        "<Leader>sB",
        function()
          Snacks.picker.grep_buffers()
        end,
        desc = "Grep open buffers",
      },
      { "<Leader>sg", Util.pick("live_grep"), desc = "Grep (root)" },
      { "<Leader>sG", Util.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
      {
        "<Leader>sp",
        function()
          Snacks.picker.lazy()
        end,
        desc = "Search for plugin spec",
      },
      { "<Leader>sw", Util.pick("grep_word"), desc = "Visual selection or word (root)", mode = { "n", "x" } },
      {
        "<Leader>sW",
        Util.pick("grep_word", { root = false }),
        desc = "Visual selection or word (cwd)",
        mode = { "n", "x" },
      },
      {
        '<Leader>s"',
        function()
          Snacks.picker.registers()
        end,
        desc = "Registers",
      },
      {
        "<Leader>sa",
        function()
          Snacks.picker.autocmds()
        end,
        desc = "Autocmds",
      },
      {
        "<Leader>sc",
        function()
          Snacks.picker.command_history()
        end,
        desc = "Command history",
      },
      {
        "<Leader>sC",
        function()
          Snacks.picker.commands()
        end,
        desc = "Commands",
      },
      {
        "<Leader>sd",
        function()
          Snacks.picker.diagnostics()
        end,
        desc = "Diagnostics",
      },
      {
        "<Leader>sh",
        function()
          Snacks.picker.help()
        end,
        desc = "Help pages",
      },
      {
        "<Leader>sH",
        function()
          Snacks.picker.highlights()
        end,
        desc = "Highlights",
      },
      {
        "<Leader>si",
        function()
          Snacks.picker.icons()
        end,
        desc = "Icons",
      },
      {
        "<Leader>sj",
        function()
          Snacks.picker.jumps()
        end,
        desc = "Jumps",
      },
      {
        "<Leader>sk",
        function()
          Snacks.picker.keymaps()
        end,
        desc = "Keymaps",
      },
      {
        "<Leader>sl",
        function()
          Snacks.picker.loclist()
        end,
        desc = "Location list",
      },
      {
        "<Leader>sM",
        function()
          Snacks.picker.man()
        end,
        desc = "Man pages",
      },
      {
        "<Leader>sm",
        function()
          Snacks.picker.marks()
        end,
        desc = "Marks",
      },
      {
        "<Leader>sR",
        function()
          Snacks.picker.resume()
        end,
        desc = "Resume",
      },
      {
        "<Leader>sq",
        function()
          Snacks.picker.qflist()
        end,
        desc = "Quickfix list",
      },
      {
        "<Leader>su",
        function()
          Snacks.picker.undo()
        end,
        desc = "Undotree",
      },
      {
        "<Leader>uC",
        function()
          Snacks.picker.colorschemes()
        end,
        desc = "Colorschemes",
      },
    },
  },
  {
    "folke/snacks.nvim",
    opts = function(_, opts)
      if Util.plugin.has("trouble.nvim") then
        return vim.tbl_deep_extend("force", opts or {}, {
          picker = {
            actions = require("trouble.sources.snacks").actions,
            win = {
              input = {
                keys = {
                  ["<C-t>"] = {
                    "trouble_open",
                    mode = { "n", "i" },
                  },
                },
              },
            },
          },
        })
      end
    end,
  },
  {
    "neovim/nvim-lspconfig",
    opts = function()
      if Util.pick.want() ~= "snacks" then
        return
      end
      local Keys = require("plugins.lsp.keymaps").get()
      vim.list_extend(Keys, {
        {
          "gd",
          function()
            Snacks.picker.lsp_definitions()
          end,
          desc = "Goto definition",
          has = "definition",
        },
        {
          "gr",
          function()
            Snacks.picker.lsp_references()
          end,
          nowait = true,
          desc = "References",
        },
        {
          "gI",
          function()
            Snacks.picker.lsp_implementations()
          end,
          desc = "Goto implementation",
        },
        {
          "gy",
          function()
            Snacks.picker.lsp_type_definitions()
          end,
          desc = "Goto T[y]pe definition",
        },
        {
          "<Leader>ss",
          function()
            Snacks.picker.lsp_symbols({ filter = Util.config.kind_filter })
          end,
          desc = "LSP symbols",
          has = "documentSymbol",
        },
        {
          "<Leader>sS",
          function()
            Snacks.picker.lsp_workspace_symbols({ filter = Util.config.kind_filter })
          end,
          desc = "LSP workspace symbols",
          has = "workspace/symbols",
        },
      })
    end,
  },
  {
    "folke/todo-comments.nvim",
    optional = true,
    keys = {
      {
        "<Leader>st",
        function()
          Snacks.picker.todo_comments()
        end,
        desc = "Todo",
      },
      {
        "<Leader>sT",
        function()
          Snacks.picker.todo_comments({ keywords = { "TODO", "FIX", "FIXME" } })
        end,
        desc = "Todo/Fix/Fixme",
      },
    },
  },
}
