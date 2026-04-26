-- lua/config/plugins/lang/prisma.lua
return Lib.lang.setup({
  cmd = "node",
  mason = { "prisma-language-server" },
  parsers = { "prisma" },
  servers = { prismals = {} },
})
