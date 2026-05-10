return Lib.lang.setup({
  cmd = "docker",
  mason = { "dockerfile-language-server", "docker-compose-language-service", "hadolint" },
  parsers = { "dockerfile" },
  servers = {
    dockerls = {},
    docker_compose_language_service = {},
  },
  linters = { dockerfile = { "hadolint" } },
})
