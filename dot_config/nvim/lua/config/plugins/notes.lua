-- lua/config/plugins/notes.lua
-- Zettelkasten note-taking via the `zk` CLI (https://github.com/zk-org/zk).
-- Requires ZK_NOTEBOOK_DIR in the environment.

return {
  {
    "zk-org/zk-nvim",
    name = "zk-nvim",
    ft   = "markdown",
    cmd  = { "ZkIndex", "ZkNew", "ZkNotes", "ZkBuffers", "ZkBacklinks", "ZkLinks", "ZkTags", "ZkCd", "ZkMatch", "ZkInsertLink" },
    keys = {
      { "<Leader>zi", function() vim.cmd("ZkIndex") end,                            desc = "Index notes" },
      { "<Leader>zz", function() vim.cmd("ZkNotes { sort = { 'modified' } }") end,  desc = "Find notes" },
      { "<Leader>zf", function() vim.cmd("ZkBuffers") end,                          desc = "Note buffers" },
      { "<Leader>zb", function() vim.cmd("ZkBacklinks") end,                        desc = "Backlinks" },
      { "<Leader>zl", function() vim.cmd("ZkLinks") end,                            desc = "Outbound links" },
      { "<Leader>zt", function() vim.cmd("ZkTags") end,                             desc = "Find by tags" },
      { "<Leader>zc", function() vim.cmd("ZkCd") end,                               desc = "Cd into notebook" },
      { "<Leader>zm", function() vim.cmd("ZkMatch") end,         mode = "x",        desc = "Note from selection" },
      { "<Leader>zL", function() vim.cmd("ZkInsertLink") end,                       desc = "Insert link" },
      {
        "<Leader>zn",
        function()
          vim.ui.input({ prompt = "Title: " }, function(input)
            if input then require("zk").new({ title = input }) end
          end)
        end,
        desc = "New note",
      },
      {
        "<Leader>zj",
        function()
          vim.ui.select({ "today", "yesterday", "tomorrow" }, { prompt = "Open journal:" }, function(choice)
            if not choice then return end
            local sep = package.config:sub(1, 1)
            require("zk").new({
              date  = choice == "today" and false or choice,
              dir   = (vim.env.ZK_NOTEBOOK_DIR or "") .. sep .. "journal",
              group = "journal",
            })
          end)
        end,
        desc = "New journal note",
      },
    },
    opts = {
      -- zk-nvim ships a snacks.picker backend. Use it explicitly — the
      -- default "select" fallback routes through vim.ui.select which
      -- isn't as rich as the native source.
      picker = "snacks_picker",
      lsp = {
        config      = { cmd = { "zk", "lsp" }, name = "zk" },
        auto_attach = { enabled = true, filetypes = { "markdown" } },
      },
    },
    config = function(_, opts) require("zk").setup(opts) end,
  },
}
