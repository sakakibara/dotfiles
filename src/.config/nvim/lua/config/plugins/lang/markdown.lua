vim.filetype.add({ extension = { mdx = "markdown.mdx" } })

vim.api.nvim_create_user_command("MarkdownPreview", function() Lib.preview.open(0) end, { desc = "Markdown preview" })
vim.api.nvim_create_user_command("MarkdownPreviewStop", function() Lib.preview.close(0) end, { desc = "Stop markdown preview" })
vim.api.nvim_create_user_command("MarkdownPreviewToggle", function() Lib.preview.toggle(0) end, { desc = "Toggle markdown preview" })

vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("LibMarkdownPreview", { clear = true }),
  pattern = "markdown",
  callback = function(ev)
    vim.keymap.set("n", "<Leader>cp", function() Lib.preview.toggle(ev.buf) end, { buffer = ev.buf, desc = "Markdown preview" })
  end,
})

return Lib.lang.setup({
  ft = { "markdown", "markdown.mdx" },
  mason = { "marksman", "prettier", "markdownlint-cli2", "markdown-toc" },
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
      "MeanderingProgrammer/render-markdown.nvim",
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
      ft   = "markdown",
      opts = {},
    },
  },
})
