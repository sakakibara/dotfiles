-- tests/helpers.lua
local M = {}
M._fails = {}
M._passes = 0

function M.describe(group_name, fn)
  print("\n" .. group_name)
  fn()
end

function M.it(name, fn)
  local ok, err = xpcall(fn, debug.traceback)
  if ok then
    M._passes = M._passes + 1
    print("  ✓ " .. name)
  else
    table.insert(M._fails, { name = name, err = err })
    print("  ✗ " .. name)
    for line in tostring(err):gmatch("[^\n]+") do print("      " .. line) end
  end
end

function M.eq(a, b, msg)
  if not vim.deep_equal(a, b) then
    error((msg or "eq") .. "\n  expected: " .. vim.inspect(b) .. "\n  got:      " .. vim.inspect(a))
  end
end

function M.truthy(v, msg)
  if not v then error((msg or "truthy") .. ": got " .. vim.inspect(v)) end
end

function M.report_and_exit()
  print(string.format("\n%d passed, %d failed", M._passes, #M._fails))
  os.exit(#M._fails == 0 and 0 or 1)
end

return M
