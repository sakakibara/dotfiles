-- lua/config/plugins/testing.lua
return {
  {
    "nvim-neotest/neotest",
    name = "neotest",
    dependencies = { "nvim-nio", "plenary.nvim", "trouble.nvim" },
    keys = {
      { "<leader>tt", function() require("neotest").run.run() end, desc = "Run nearest" },
      { "<leader>tT", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run file" },
      { "<leader>tr", function() require("neotest").run.run_last() end, desc = "Run last" },
      { "<leader>ts", function() require("neotest").summary.toggle() end, desc = "Toggle summary" },
      { "<leader>to", function() require("neotest").output.open({ enter = true, auto_close = true }) end, desc = "Open output" },
      { "<leader>tO", function() require("neotest").output_panel.toggle() end, desc = "Toggle output panel" },
      { "<leader>tS", function() require("neotest").run.stop() end, desc = "Stop" },
      { "<leader>tw", function() require("neotest").watch.toggle(vim.fn.expand("%")) end, desc = "Watch file" },
      { "<leader>td", function() require("neotest").run.run({ strategy = "dap" }) end, desc = "Debug nearest" },
    },
    config = function()
      require("neotest").setup({
        adapters = Lib.neotest.list(),
        status = { virtual_text = true },
        output = { open_on_run = true },
        quickfix = {
          open = function()
            local ok, trouble = pcall(require, "trouble")
            if ok then trouble.open({ mode = "quickfix", focus = false })
            else vim.cmd("copen") end
          end,
        },
      })
    end,
  },

  {
    "folke/trouble.nvim",
    name = "trouble.nvim",
    cmd = "Trouble",
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<cr>", desc = "Diagnostics (Trouble)" },
      { "<leader>xX", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>", desc = "Buffer diagnostics" },
      { "<leader>xL", "<cmd>Trouble lsp toggle<cr>", desc = "LSP refs (Trouble)" },
    },
    opts = {
      auto_close = true,
      modes = {
        lsp_document_symbols = { format = "{kind_icon}{symbol.name}" },
      },
    },
  },
}
