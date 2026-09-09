return Lib.lang.setup({
  cmd = "thrift",
  ft = "thrift",
  mason = { "thriftls" },
  parsers = { "thrift" },
  servers = { thriftls = {} },
})
