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
}
