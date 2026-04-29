-- tests/pack/fixtures.lua
-- Reusable spec factories so per-test setup stays small.
local M = {}

function M.minimal_eager(name)
  return { dev = true, name = name or "foo" }
end

function M.minimal_lazy(name)
  return { dev = true, name = name or "foo", event = "User Bar" }
end

function M.with_keys(name, lhs)
  return {
    dev = true, name = name or "foo",
    keys = { { lhs or "<F60>", function() end, mode = "n" } },
  }
end

function M.with_opts(name, opts)
  return { dev = true, name = name or "foo", opts = opts or {} }
end

function M.with_version(name, v)
  return { dev = true, name = name or "foo", version = v }
end

function M.with_deps(name, deps)
  return { dev = true, name = name or "foo", dependencies = deps or {} }
end

return M
