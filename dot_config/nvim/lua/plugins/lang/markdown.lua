local unotes = require("util.notes")
local upath = require("util.path")
local utelescope = require("util.telescope")

return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        vim.list_extend(opts.ensure_installed, { "markdown", "markdown_inline" })
      end
    end,
  },

  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, { "markdownlint", "marksman" })
    end,
  },

  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = {
      linters_by_ft = {
        markdown = { "markdownlint" },
      },
    },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        marksman = {},
      },
    },
  },

  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
    keys = {
      {
        "<leader>cp",
        ft = "markdown",
        "<cmd>MarkdownPreviewToggle<cr>",
        desc = "Markdown Preview",
      },
    },
    config = function()
      vim.cmd([[do FileType]])
    end,
  },

  {
    "lukas-reineke/headlines.nvim",
    opts = function()
      local opts = {}
      for _, ft in ipairs({ "markdown", "norg", "rmd", "org" }) do
        opts[ft] = {
          headline_highlights = {},
          fat_headlines = false,
          bullets = { "◎", "○", "✺", "▶", "⤷" },
        }
        for i = 1, 6 do
          local hl = "Headline" .. i
          vim.api.nvim_set_hl(0, hl, { link = "Headline", default = true })
          table.insert(opts[ft].headline_highlights, hl)
        end
      end
      return opts
    end,
    ft = { "markdown", "norg", "rmd", "org" },
    config = function(_, opts)
      vim.schedule(function()
        require("headlines").setup(opts)
        require("headlines").refresh()
      end)
    end,
  },

  {
    "nvim-telescope/telescope.nvim",
    keys = {
      {
        "<leader>nn",
        function()
          vim.ui.input({ prompt = "Title:" }, function(input)
            if input then
              vim.schedule(function()
                vim.api.nvim_cmd({ cmd = "edit", args = { unotes.notes_root .. upath.sep .. input .. ".md" } }, {})
              end)
            end
          end)
        end,
        desc = "Edit markdown file",
      },
      { "<leader>nf", utelescope.func("files", { cwd = unotes.notes_root }), desc = "Files (notes)" },
      { "<leader>ng", utelescope.func("live_grep", { cwd = unotes.notes_root }), desc = "Grep (notes)" },
      {
        "<leader>nj",
        function()
          vim.ui.select({ "today", "yesterday", "tomorrow" }, {
            prompt = "Open journal:",
          }, function(choice)
            if choice then
              local time = os.time()
              if choice == "yesterday" then
                time = time - 24 * 60 * 60
              elseif choice == "tomorrow" then
                time = time + 24 * 60 * 60
              end
              vim.schedule(function()
                unotes.open_journal(time)
              end)
            end
          end)
        end,
        desc = "Create journal note",
      },
    },
  },

  {
    "nvim-telescope/telescope-live-grep-args.nvim",
    keys = {
      {
        "<leader>na",
        utelescope.func("live_grep_args", { cwd = unotes.notes_root }),
        desc = "Grep with args (notes)",
      },
    },
  },

  {
    "nfrid/due.nvim",
    opts = {},
  },
}
