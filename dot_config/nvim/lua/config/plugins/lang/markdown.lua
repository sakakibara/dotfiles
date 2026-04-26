-- lua/config/plugins/lang/markdown.lua
-- markdown-toc not in mason registry; install via `npm i -g markdown-toc` if desired.
return Lib.lang.setup({
  mason = { "marksman", "prettier", "markdownlint-cli2" },
  parsers = { "markdown", "markdown_inline" },
  servers = { marksman = {} },
  formatters = {
    markdown = { "prettier", "markdownlint-cli2", "markdown-toc" },
    ["markdown.mdx"] = { "prettier", "markdownlint-cli2", "markdown-toc" },
  },
  formatters_setup = function(conform)
    conform.formatters = conform.formatters or {}
    conform.formatters["markdown-toc"] = {
      condition = function(_, ctx)
        for _, line in ipairs(vim.api.nvim_buf_get_lines(ctx.buf, 0, -1, false)) do
          if line:find("<!%-%- toc %-%->") then return true end
        end
      end,
    }
    conform.formatters["markdownlint-cli2"] = {
      condition = function(_, ctx)
        local diag = vim.tbl_filter(function(d)
          return d.source == "markdownlint"
        end, vim.diagnostic.get(ctx.buf))
        return #diag > 0
      end,
    }
  end,
  linters = { markdown = { "markdownlint-cli2" } },
  plugins = {
    {
      "iamcco/markdown-preview.nvim",
      name = "markdown-preview.nvim",
      cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
      ft = "markdown",
      keys = {
        { "<Leader>cp", "<Cmd>MarkdownPreviewToggle<CR>", ft = "markdown", desc = "Markdown Preview" },
      },
      -- NOTE: requires `cd app && npm install` in the plugin dir on first install,
      -- or `:call mkdp#util#install()`. Not auto-run — `build` isn't a core.pack field.
      config = function()
        if vim.fn["mkdp#util#install"] and vim.fn.executable("node") == 1 then
          local plugin_dir = vim.fn.stdpath("data") .. "/site/pack/core/opt/markdown-preview.nvim"
          local marker = plugin_dir .. "/app/bin/markdown-preview-" .. (vim.loop.os_uname().sysname:lower())
          if vim.fn.glob(marker .. "*") == "" then
            pcall(vim.fn["mkdp#util#install"])
          end
        end
        vim.cmd([[do FileType]])
      end,
    },

    {
      "MeanderingProgrammer/render-markdown.nvim",
      name = "render-markdown.nvim",
      ft = { "markdown" },
      opts = {
        heading = {
          enabled = false,
        },
      },
    },

    -- Due dates: highlights `@due(YYYY-MM-DD)` style annotations
    {
      "nfrid/due.nvim",
      name = "due.nvim",
      ft   = "markdown",
      opts = {},
    },
  },
})
