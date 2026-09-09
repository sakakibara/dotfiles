vim.filetype.add({
  pattern = {
    [".*/docker%-compose%.ya?ml"] = "yaml.docker-compose",
    [".*/docker%-compose%.[^/]+%.ya?ml"] = "yaml.docker-compose",
    [".*/compose%.ya?ml"] = "yaml.docker-compose",
    [".*/compose%.[^/]+%.ya?ml"] = "yaml.docker-compose",
  },
})

Lib.parsers.add("yaml", { ft = "yaml.docker-compose" })

return Lib.lang.setup({
  cmd = "docker",
  ft = { "dockerfile", "yaml.docker-compose" },
  no_parser = { "yaml.docker-compose" },
  mason = { "dockerfile-language-server", "docker-compose-language-service", "hadolint" },
  parsers = { "dockerfile" },
  servers = {
    dockerls = {},
    docker_compose_language_service = {},
  },
  linters = { dockerfile = { "hadolint" } },
})
