-- thriftls is not in the mason registry; install manually if needed.
return Lib.lang.setup({
  cmd = "thrift",
  parsers = { "thrift" },
  servers = { thriftls = {} },
})
