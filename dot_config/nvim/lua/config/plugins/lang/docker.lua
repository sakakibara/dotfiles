-- lua/config/plugins/lang/docker.lua
if vim.fn.executable("docker") == 0 then return {} end

Lib.mason.add("dockerfile-language-server", "docker-compose-language-service", "hadolint")

Lib.plugin.on_load("nvim-treesitter", function()
  require("nvim-treesitter.install").ensure_installed({ "dockerfile" })
end)

Lib.plugin.on_load("nvim-lspconfig", function()
  vim.lsp.config("dockerls", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("dockerls")
  vim.lsp.config("docker_compose_language_service", { capabilities = Lib.lsp.capabilities() })
  vim.lsp.enable("docker_compose_language_service")
end)

Lib.plugin.on_load("nvim-lint", function()
  require("lint").linters_by_ft.dockerfile = { "hadolint" }
end)

return {}
