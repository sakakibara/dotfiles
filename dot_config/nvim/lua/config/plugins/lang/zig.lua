-- lua/config/plugins/lang/zig.lua
if vim.fn.executable("zig") == 0 then return {} end

Lib.mason.add("zls")

Lib.neotest.add("neotest-zig", function() return require("neotest-zig") end)

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "zig" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("zls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("zls")
end)

return {
  {
    "lawrence-laz/neotest-zig",
    name = "neotest-zig",
    ft = "zig",
    dependencies = { "neotest" },
  },
}
