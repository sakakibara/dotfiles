-- lua/config/plugins/lang/markdown.lua
Lib.mason.add("marksman", "prettier", "markdownlint-cli2")
-- markdown-toc not in mason registry; user installs via `npm i -g markdown-toc` if desired.

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter").install({ "markdown", "markdown_inline" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("marksman", { capabilities = Lib.lsp.capabilities() })
  Lib.lsp.enable("marksman")
end)

Lib.plugin.on_load("conform.nvim", function()
  local conform = require("conform")
  conform.formatters = conform.formatters or {}
  conform.formatters["markdown-toc"] = {
    condition = function(_, ctx)
      for _, line in ipairs(vim.api.nvim_buf_get_lines(ctx.buf, 0, -1, false)) do
        if line:find("<!%-%- toc %-%->") then
          return true
        end
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
  conform.formatters_by_ft["markdown"] = { "prettier", "markdownlint-cli2", "markdown-toc" }
  conform.formatters_by_ft["markdown.mdx"] = { "prettier", "markdownlint-cli2", "markdown-toc" }
end)

Lib.plugin.on_load("nvim-lint", function()
  require("lint").linters_by_ft.markdown = { "markdownlint-cli2" }
end)

return {
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
        -- Lazy: only run the first time, if the built binary is missing.
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
}
