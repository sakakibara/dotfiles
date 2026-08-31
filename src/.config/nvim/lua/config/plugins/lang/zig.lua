return Lib.lang.setup({
  cmd = "zig",
  ft = "zig",
  mason = { "zls" },
  parsers = { "zig" },
  servers = { zls = {} },
  neotest = { ["neotest-zig"] = function() return require("neotest-zig") end },
  plugins = {
    {
      "lawrence-laz/neotest-zig",
      ft = "zig",
      dependencies = { "neotest" },
    },
  },
})
