return {
  {
    "stevearc/conform.nvim",
    optional = true,
    opts = {
      formatters_by_ft = {
        ["markdown"] = { { "prettierd", "prettier" }, "markdownlint", "markdown-toc" },
        ["markdown.mdx"] = { { "prettierd", "prettier" }, "markdownlint", "markdown-toc" },
      },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "markdown", "markdown_inline" } },
  },

  {
    "williamboman/mason.nvim",
    opts = { ensure_installed = { "markdownlint", "markdown-toc" } },
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
        "<Leader>cp",
        ft = "markdown",
        "<Cmd>MarkdownPreviewToggle<CR>",
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
        "<Leader>nn",
        function()
          vim.ui.input({ prompt = "Title:" }, function(input)
            if input then
              vim.schedule(function()
                vim.api.nvim_cmd(
                  { cmd = "edit", args = { Util.notes.notes_root .. Util.path.sep .. input .. ".md" } },
                  {}
                )
              end)
            end
          end)
        end,
        desc = "Edit markdown file",
      },
      { "<Leader>nf", Util.telescope.run("files", { cwd = Util.notes.notes_root }), desc = "Files (notes)" },
      { "<Leader>ng", Util.telescope.run("live_grep", { cwd = Util.notes.notes_root }), desc = "Grep (notes)" },
      {
        "<Leader>nj",
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
                Util.notes.open_journal(time)
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
        "<Leader>na",
        Util.telescope.run("live_grep_args", { cwd = Util.notes.notes_root }),
        desc = "Grep with args (notes)",
      },
    },
  },

  {
    "nfrid/due.nvim",
    opts = {},
  },
}
