return Lib.lang.setup({
  cmd = "node",
  ft = "prisma",
  mason = { "prisma-language-server" },
  parsers = { "prisma" },
  servers = { prismals = {} },
})
